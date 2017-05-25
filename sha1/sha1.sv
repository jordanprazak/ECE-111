module sha1(input logic clk, reset_n, start,
            input logic [31:0] message_addr, size, output_addr,
           output logic done, mem_clk, mem_we,
           output logic [15:0] mem_addr,
           output logic [31:0] mem_write_data,
            input logic [31:0] mem_read_data);

// As of last time: First cycle works
// Only error happens because the size of the message is attached to the end of the second message block instead of the third

function logic [31:0] left_rot_1(input logic [31:0] value);
  left_rot_1 = {value[30:0], value[31]};
endfunction

function logic [31:0] left_rot_5(input logic [31:0] value);
  left_rot_5 = {value[26:0], value[31:27]};
endfunction

function logic [31:0] left_rot_30(input logic [31:0] value);
  left_rot_30 = {value[1:0], value[31:2]};
endfunction

function logic [159:0] hash_op(input logic [31:0] a, b, c, d, e, w,
                               input logic [7:0] t);
  logic [31:0] k, f, tee;

  if (t <= 19) begin
    k = 32'h5a827999;
    f = (b&c) ^ (~b&d);
  end
  else if (t <= 39) begin
    k = 32'h6ed9eba1;
    f = b ^ c ^ d;
  end
  else if (t<= 59) begin
    k = 32'h8f1bbcdc;
    f = (b&c) | (b&d) | (c&d);
  end
  else if (t<= 79) begin
    k = 32'hca62c1d6;
    f = b ^ c ^ d;
  end
  else begin
    k = 32'd0;
    f = 32'd0;
  end

  tee = left_rot_5(a) + f + w + k + e;

  hash_op = {tee, a, left_rot_30(b), c, d};
endfunction

// Attach the memory clock to the main clock
assign mem_clk = clk;

// Calculate the number of 512-bit blocks needed based on the message size
logic [15:0] num_blocks;
assign num_blocks = 1 + ((size + 8) >> 6);

// Use state variable to track reads and writes
enum logic [1:0] {IDLE=2'b00, COMP=2'b01, READ=2'b10, WRITE=2'b11} state;

// Create read and write counters
logic [15:0] rc, wc;

// Keep track of SHA-1 computation steps wtih t
logic [7:0] t;

// Create 16 32-bit words to hold current and past values of W
logic [31:0] w[0:15];

// Create H registers to hold hash_block outputs
logic [31:0] h0, h1, h2, h3, h4;

// Create registers to hold intermediate hash_block outputs
logic [31:0] a, b, c, d, e;

logic [31:0] w_in;
always_comb begin
  if (t <= 15) begin
    if (rc <= (size >> 2)+1) begin
      w_in = mem_read_data;
    end
    else if (rc == (size >> 2) + 2) begin
      case (size % 4)
        0: w_in = 32'h80000000;
        1: w_in = (mem_read_data & 32'hff000000) | 32'h00800000;
        2: w_in = (mem_read_data & 32'hffff0000) | 32'h00008000;
        3: w_in = (mem_read_data & 32'hffffff00) | 32'h00000080;
      endcase
    end
    else begin
      if (t == 14 && (rc >> 4) == num_blocks) begin
        w_in = {29'd0, size[31:29]};
      end
      else if (t == 15 && (rc >> 4) == num_blocks) begin
        w_in = {size[28:0], 3'd0};
      end
      else begin
        w_in = 32'd0;
      end
    end
  end
  else begin
    w_in = left_rot_1(w[2] ^ w[7] ^ w[13] ^ w[15]);
  end
end

always_ff @(posedge clk, negedge reset_n)
begin
  if (!reset_n) begin
    state <= IDLE;
  end
  else begin
    case (state)
      IDLE: begin
        if (start) begin // READ first word
          mem_we <= 0;
          mem_addr <= message_addr;
          rc <= 1;
          wc <= 0;
          h0 <= 32'h67452301;
          h1 <= 32'hefcdab89;
          h2 <= 32'h98badcfe;
          h3 <= 32'h10325476;
          h4 <= 32'hc3d2e1f0;
          a <= 32'h67452301;
          b <= 32'hefcdab89;
          c <= 32'h98badcfe;
          d <= 32'h10325476;
          e <= 32'hc3d2e1f0;
          state <= READ;
          t <= 0;
        end
      end
      READ: begin
          state <= COMP;
          mem_addr <= message_addr + rc;
          rc <= rc + 1;
      end
      COMP: begin
        if (t <= 79) begin
          state <= COMP;
          mem_we <= 0;
          mem_addr <= message_addr + rc;
          if (t < 15) begin
            rc <= rc + 1;
          end
          t <= t + 1;
          {a, b, c, d, e} <= hash_op(a, b, c, d, e, w_in, t);
          w[0] <= w_in;
          for(int i = 1; i <= 15; i++) begin
            w[i] <= w[i-1];
          end
        end
        else begin
          h0 <= h0 + a;
          h1 <= h1 + b;
          h2 <= h2 + c;
          h3 <= h3 + d;
          h4 <= h4 + e;
          a <= h0 + a;
          b <= h1 + b;
          c <= h2 + c;
          d <= h3 + d;
          e <= h4 + e;
          t <= 0;
          if ((rc >> 4)  >= num_blocks) begin
            state <= WRITE;
          end
          else begin
            mem_we <= 0;
            mem_addr <= message_addr + rc - 1;
            rc <= rc;
            state <= READ;
          end
        end
        
      end
      WRITE: begin
        mem_we <= 1;
        case (t)
          0: begin
            mem_addr <= output_addr + wc;
            mem_write_data <= h0;
            t <= 1;
          end
          1: begin
            mem_addr <= output_addr + wc;
            mem_write_data <= h1;
            t <= 2;
            state <= WRITE;
          end
          2: begin
            mem_addr <= output_addr + wc;
            mem_write_data <= h2;
            t <= 3;
            state <= WRITE;
          end
          3: begin
            mem_addr <= output_addr + wc;
            mem_write_data <= h3;
            t <= 4;
            state <= WRITE;
          end
          4: begin
            mem_addr <= output_addr + wc;
            mem_write_data <= h4;
            t <= 5;
            state <= WRITE;
          end
          5: begin
            mem_we <= 0;
            state <= IDLE;
            done <= 1;
          end
        endcase // t
        wc <= wc + 1;
      end
    endcase // state
  end
end

endmodule