module sha256(input logic clk, reset_n, start,
              input logic [31:0] message_addr, size, output_addr,
             output logic done, mem_clk, mem_we,
             output logic [15:0] mem_addr,
             output logic [31:0] mem_write_data,
              input logic [31:0] mem_read_data);

function logic [31:0] right_rot(input logic [31:0] value, input logic [5:0] rot_amt);
  right_rot = (value >> rot_amt) | (value << (6'd32-rot_amt));
endfunction

// SHA256 K constants
parameter int k[0:63] = '{
   32'h428a2f98, 32'h71374491, 32'hb5c0fbcf, 32'he9b5dba5, 32'h3956c25b, 32'h59f111f1, 32'h923f82a4, 32'hab1c5ed5,
   32'hd807aa98, 32'h12835b01, 32'h243185be, 32'h550c7dc3, 32'h72be5d74, 32'h80deb1fe, 32'h9bdc06a7, 32'hc19bf174,
   32'he49b69c1, 32'hefbe4786, 32'h0fc19dc6, 32'h240ca1cc, 32'h2de92c6f, 32'h4a7484aa, 32'h5cb0a9dc, 32'h76f988da,
   32'h983e5152, 32'ha831c66d, 32'hb00327c8, 32'hbf597fc7, 32'hc6e00bf3, 32'hd5a79147, 32'h06ca6351, 32'h14292967,
   32'h27b70a85, 32'h2e1b2138, 32'h4d2c6dfc, 32'h53380d13, 32'h650a7354, 32'h766a0abb, 32'h81c2c92e, 32'h92722c85,
   32'ha2bfe8a1, 32'ha81a664b, 32'hc24b8b70, 32'hc76c51a3, 32'hd192e819, 32'hd6990624, 32'hf40e3585, 32'h106aa070,
   32'h19a4c116, 32'h1e376c08, 32'h2748774c, 32'h34b0bcb5, 32'h391c0cb3, 32'h4ed8aa4a, 32'h5b9cca4f, 32'h682e6ff3,
   32'h748f82ee, 32'h78a5636f, 32'h84c87814, 32'h8cc70208, 32'h90befffa, 32'ha4506ceb, 32'hbef9a3f7, 32'hc67178f2
};

function logic [159:0] hash_op(input logic [31:0] a, b, c, d, e, f, g, h, w,
                               input logic [7:0] t);
  logic [31:0] s0, s1, maj, t2, ch, t1;

  s0 = right_rot(a, 6'd2) ^ right_rot(a, 6'd13) ^ right_rot(a, 6'd22);

  maj = (a&b) ^ (a&c) ^ (b&c);

  t2 = s0 + maj;

  s1 = right_rot(e, 6'd6) ^ right_rot(e, 6'd11) ^ right_rot(e, 6'd25);

  ch = (e&f) ^ (~e&g);

  t1 = h + s1 + ch + k[t] + w;
  
  hash_op = {t1+t2, a, b, c, d+t1, e, f, g};
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
logic [31:0] h0, h1, h2, h3, h4, h5, h6, h7;

// Create registers to hold intermediate hash_block outputs
logic [31:0] a, b, c, d, e, f, g, h;

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
    //s0 = right_rot(w[14], 6'd7) ^ right_rot(w[14], 6'd18) ^ (w[14] >> 3);
    //s1 = right_rot(w[1], 6'd17) ^ right_rot(w[1], 6'd19) ^ (w[1] >> 10);
    w_in = w[15] + right_rot(w[14], 6'd7) ^ right_rot(w[14], 6'd18) ^ (w[14] >> 3) + w[6] + right_rot(w[1], 6'd17) ^ right_rot(w[1], 6'd19) ^ (w[1] >> 10);
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
          h0 <= 32'h6a09e667;
          h1 <= 32'hbb67ae85;
          h2 <= 32'h3c6ef372;
          h3 <= 32'ha54ff53a;
          h4 <= 32'h510e527f;
          h5 <= 32'h9b05688c;
          h6 <= 32'h1f83d9ab;
          h7 <= 32'h5be0cd19;
          a <= 32'h6a09e667;
          b <= 32'hbb67ae85;
          c <= 32'h3c6ef372;
          d <= 32'ha54ff53a;
          e <= 32'h510e527f;
          f <= 32'h9b05688c;
          g <= 32'h1f83d9ab;
          h <= 32'h5be0cd19;
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
        if (t <= 63) begin // change to 63 for sha256
          state <= COMP;
          mem_we <= 0;
          mem_addr <= message_addr + rc;
          if (t < 15) begin
            rc <= rc + 1;
          end
          t <= t + 1;
          {a, b, c, d, e, f, g, h} <= hash_op(a, b, c, d, e, f, g, h, w_in, t);
          w[15] <= w_in;
            for(int i = 14; i >= 0; i--) begin
              w[i] <= w[i+1];
            end
        end
        else begin
          h0 <= h0 + a;
          h1 <= h1 + b;
          h2 <= h2 + c;
          h3 <= h3 + d;
          h4 <= h4 + e;
          h5 <= h5 + f;
          h6 <= h6 + g;
          h7 <= h7 + h;
          a <= h0 + a;
          b <= h1 + b;
          c <= h2 + c;
          d <= h3 + d;
          e <= h4 + e;
          f <= h5 + f;
          g <= h6 + g;
          h <= h7 + h;
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
            mem_addr <= output_addr + wc;
            mem_write_data <= h5;
            t <= 6;
            state <= WRITE;
          end
          6: begin
            mem_addr <= output_addr + wc;
            mem_write_data <= h6;
            t <= 7;
            state <= WRITE;
          end
          7: begin
            mem_addr <= output_addr + wc;
            mem_write_data <= h7;
            t <= 8;
            state <= WRITE;
          end
          8: begin
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