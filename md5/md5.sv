module md5(input logic clk, reset_n, start,
            input logic [31:0] message_addr, size, output_addr,
           output logic done, mem_clk, mem_we,
           output logic [15:0] mem_addr,
           output logic [31:0] mem_write_data,
            input logic [31:0] mem_read_data);
// MD5 K constants
parameter int k[0:63] = '{
    32'hd76aa478, 32'he8c7b756, 32'h242070db, 32'hc1bdceee,
    32'hf57c0faf, 32'h4787c62a, 32'ha8304613, 32'hfd469501,
    32'h698098d8, 32'h8b44f7af, 32'hffff5bb1, 32'h895cd7be,
    32'h6b901122, 32'hfd987193, 32'ha679438e, 32'h49b40821,
    32'hf61e2562, 32'hc040b340, 32'h265e5a51, 32'he9b6c7aa,
    32'hd62f105d, 32'h02441453, 32'hd8a1e681, 32'he7d3fbc8,
    32'h21e1cde6, 32'hc33707d6, 32'hf4d50d87, 32'h455a14ed,
    32'ha9e3e905, 32'hfcefa3f8, 32'h676f02d9, 32'h8d2a4c8a,
    32'hfffa3942, 32'h8771f681, 32'h6d9d6122, 32'hfde5380c,
    32'ha4beea44, 32'h4bdecfa9, 32'hf6bb4b60, 32'hbebfbc70,
    32'h289b7ec6, 32'heaa127fa, 32'hd4ef3085, 32'h04881d05,
    32'hd9d4d039, 32'he6db99e5, 32'h1fa27cf8, 32'hc4ac5665,
    32'hf4292244, 32'h432aff97, 32'hab9423a7, 32'hfc93a039,
    32'h655b59c3, 32'h8f0ccc92, 32'hffeff47d, 32'h85845dd1,
    32'h6fa87e4f, 32'hfe2ce6e0, 32'ha3014314, 32'h4e0811a1,
    32'hf7537e82, 32'hbd3af235, 32'h2ad7d2bb, 32'heb86d391
};

// MD5 S constants
parameter byte S[0:63] = '{
    8'd7, 8'd12, 8'd17, 8'd22, 8'd7, 8'd12, 8'd17, 8'd22, 8'd7, 8'd12, 8'd17, 8'd22, 8'd7, 8'd12, 8'd17, 8'd22,
    8'd5, 8'd9,  8'd14, 8'd20, 8'd5, 8'd9,  8'd14, 8'd20, 8'd5, 8'd9,  8'd14, 8'd20, 8'd5, 8'd9,  8'd14, 8'd20,
    8'd4, 8'd11, 8'd16, 8'd23, 8'd4, 8'd11, 8'd16, 8'd23, 8'd4, 8'd11, 8'd16, 8'd23, 8'd4, 8'd11, 8'd16, 8'd23,
    8'd6, 8'd10, 8'd15, 8'd21, 8'd6, 8'd10, 8'd15, 8'd21, 8'd6, 8'd10, 8'd15, 8'd21, 8'd6, 8'd10, 8'd15, 8'd21
};


function logic [31:0] left_rot_1(input logic [31:0] value);
  left_rot_1 = {value[30:0], value[31]};
endfunction

function logic [31:0] left_rot_5(input logic [31:0] value);
  left_rot_5 = {value[26:0], value[31:27]};
endfunction
function logic [31:0] left_rot_30(input logic [31:0] value);
  left_rot_30 = {value[1:0], value[31:2]};
endfunction

function logic [127:0] hash_op(input logic [31:0] a, b, c, d, w,
                               input logic [7:0] t);
							   //removed e for md5
  logic [31:0] f, temp1, temp2;

  if (t <= 15) begin
    f = (b&c) | (~b&d);
  end
  else if (t <= 31) begin
    f = (d&b) | (~d&c);
  end
  else if (t <= 47) begin
    f = b ^ c ^ d;
  end
  else begin
    f = c ^ (b|~d);
  end

  temp1 = a + f + k[t] + w;
  temp2 = b + ((temp1 << S[t]) | (temp1 >> (8'd32-S[t])));

  hash_op = {d, temp2, b, c};
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

//create registers to hold intermediate hash_block outputs
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
        w_in = 32'd0;	//pad with zeros
      end
    end
  end
  else begin //generate new values of w using previous values
  	if (t <= 31) begin
		w_in = w[(5*t + 1)%16];
	end
	else if (t <= 47) begin
		w_in = w[(3*t + 5)%16];
	end
	else begin
		w_in = w[(7*t)%16];
	end
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
          //h4 <= 32'hc3d2e1f0;
          a <= 32'h67452301;
          b <= 32'hefcdab89;
          c <= 32'h98badcfe;
          d <= 32'h10325476;
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
		  if (t <= 63) begin	//changed to 63 for md5
          state <= COMP;
          mem_we <= 0;
          mem_addr <= message_addr + rc;
          if (t <= 15) begin
            rc <= rc + 1;
            w[15] <= w_in;
            for(int i = 14; i >= 0; i--) begin
              w[i] <= w[i+1];
            end
          end
          t <= t + 1;
          {a, b, c, d} <= hash_op(a, b, c, d, w_in, t);
        end
        else begin
          h0 <= h0 + a;
          h1 <= h1 + b;
          h2 <= h2 + c;
          h3 <= h3 + d;
          //h4 <= h4 + e;
          a <= h0 + a;
          b <= h1 + b;
          c <= h2 + c;
          d <= h3 + d;
          //e <= h4 + e;
          t <= 0;
          if ((rc >> 4)  >= num_blocks) begin
            state <= WRITE;
          end
          else begin
            mem_we <= 0;
            mem_addr <= message_addr + rc - 2;
            rc <= rc - 1;
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
          //4: begin
           // mem_addr <= output_addr + wc;
           // mem_write_data <= h4;
           // t <= 5;
           // state <= WRITE;
          //end
          4: begin // Changed from 5 to 4 for md5
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
