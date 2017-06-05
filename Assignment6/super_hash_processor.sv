module super_hash_processor(input logic clk, reset_n, start,
                            input logic [1:0] opcode,
                            input logic [31:0] message_addr, size, output_addr,
                           output logic done, mem_clk, mem_we,
                           output logic [15:0] mem_addr,
                           output logic [31:0] mem_write_data,
                            input logic [31:0] mem_read_data);
// MD5 K constants
parameter int md5_k[0:63] = '{
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
/*
// MD5 S constants
parameter byte md5_S[0:63] = '{
    8'd7, 8'd12, 8'd17, 8'd22, 8'd7, 8'd12, 8'd17, 8'd22, 8'd7, 8'd12, 8'd17, 8'd22, 8'd7, 8'd12, 8'd17, 8'd22,
    8'd5, 8'd9,  8'd14, 8'd20, 8'd5, 8'd9,  8'd14, 8'd20, 8'd5, 8'd9,  8'd14, 8'd20, 8'd5, 8'd9,  8'd14, 8'd20,
    8'd4, 8'd11, 8'd16, 8'd23, 8'd4, 8'd11, 8'd16, 8'd23, 8'd4, 8'd11, 8'd16, 8'd23, 8'd4, 8'd11, 8'd16, 8'd23,
    8'd6, 8'd10, 8'd15, 8'd21, 8'd6, 8'd10, 8'd15, 8'd21, 8'd6, 8'd10, 8'd15, 8'd21, 8'd6, 8'd10, 8'd15, 8'd21
};
*/
// SHA256 K constants
parameter int sha256_k[0:63] = '{
   32'h428a2f98, 32'h71374491, 32'hb5c0fbcf, 32'he9b5dba5, 32'h3956c25b, 32'h59f111f1, 32'h923f82a4, 32'hab1c5ed5,
   32'hd807aa98, 32'h12835b01, 32'h243185be, 32'h550c7dc3, 32'h72be5d74, 32'h80deb1fe, 32'h9bdc06a7, 32'hc19bf174,
   32'he49b69c1, 32'hefbe4786, 32'h0fc19dc6, 32'h240ca1cc, 32'h2de92c6f, 32'h4a7484aa, 32'h5cb0a9dc, 32'h76f988da,
   32'h983e5152, 32'ha831c66d, 32'hb00327c8, 32'hbf597fc7, 32'hc6e00bf3, 32'hd5a79147, 32'h06ca6351, 32'h14292967,
   32'h27b70a85, 32'h2e1b2138, 32'h4d2c6dfc, 32'h53380d13, 32'h650a7354, 32'h766a0abb, 32'h81c2c92e, 32'h92722c85,
   32'ha2bfe8a1, 32'ha81a664b, 32'hc24b8b70, 32'hc76c51a3, 32'hd192e819, 32'hd6990624, 32'hf40e3585, 32'h106aa070,
   32'h19a4c116, 32'h1e376c08, 32'h2748774c, 32'h34b0bcb5, 32'h391c0cb3, 32'h4ed8aa4a, 32'h5b9cca4f, 32'h682e6ff3,
   32'h748f82ee, 32'h78a5636f, 32'h84c87814, 32'h8cc70208, 32'h90befffa, 32'ha4506ceb, 32'hbef9a3f7, 32'hc67178f2
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
function logic [31:0] left_rotate_S(input logic [31:0] x,
                                   input logic [6:0] t);
  logic [3:0] i;
  i = {t[5:4], t[1:0]};
  case (i)
  0: left_rotate_S = {x[24:0], x[31:25]}; // leftrotate S[t] = 7
  1: left_rotate_S = {x[19:0], x[31:20]}; // leftrotate S[t] = 12
  2: left_rotate_S = {x[14:0], x[31:15]}; // leftrotate S[t] = 17
  3: left_rotate_S = {x[9:0], x[31:10]}; // leftrotate S[t] = 22
  4: left_rotate_S = {x[26:0], x[31:27]}; // leftrotate S[t] = 5
  5: left_rotate_S = {x[22:0], x[31:23]}; // leftrotate S[t] = 9
  6: left_rotate_S = {x[17:0], x[31:18]}; // leftrotate S[t] = 14
  7: left_rotate_S = {x[11:0], x[31:12]}; // leftrotate S[t] = 20
  8: left_rotate_S = {x[27:0], x[31:28]}; // leftrotate S[t] = 4
  9: left_rotate_S = {x[20:0], x[31:21]}; // leftrotate S[t] = 11
  10: left_rotate_S = {x[15:0], x[31:16]}; // leftrotate S[t] = 16
  11: left_rotate_S = {x[8:0], x[31:9]}; // leftrotate S[t] = 23
  12: left_rotate_S = {x[25:0], x[31:26]}; // leftrotate S[t] = 6
  13: left_rotate_S = {x[21:0], x[31:22]}; // leftrotate S[t] = 10
  14: left_rotate_S = {x[16:0], x[31:17]}; // leftrotate S[t] = 15
  default: left_rotate_S = {x[10:0], x[31:11]}; // leftrotate S[t] = 21
  endcase
endfunction


function logic [159:0] sha1_hash_op(input logic [31:0] a, b, c, d, e, w,
                                    input logic [7:0] t);
  logic [31:0] k, f, temp;

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

  temp = left_rot_5(a) + f + w + k + e;

  sha1_hash_op = {temp, a, left_rot_30(b), c, d};
endfunction

function logic [127:0] md5_hash_op(input logic [31:0] a, b, c, d, w,
                                   input logic [6:0] t);
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

  temp1 = a + f + md5_k[t] + w;
  temp2 = b + left_rotate_S(temp1, t);

  md5_hash_op = {d, temp2, b, c};
endfunction

function logic [255:0] sha256_hash_op(input logic [31:0] a, b, c, d, e, f, g, h, w,
                                      input logic [7:0] t);
  logic [31:0] s0, s1, maj, t2, ch, t1;

  s0 = ((a >> 2) | (a << 30)) ^ ((a >> 13) | (a << 19)) ^ ((a >> 22) | (a << 10));
  maj = (a&b) | (a&c) | (b&c);
  t2 = s0 + maj;

  s1 = ((e >> 6) | (e << 26)) ^ ((e >> 11) | (e << 21)) ^ ((e >> 25) | (e << 7));
  ch = (e&f) | (~e&g);
  t1 = h + s1 + ch + sha256_k[t] + w;

  sha256_hash_op = {t1+t2, a, b, c, d+t1, e, f, g};
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
logic [6:0] t;

// Create 16 32-bit words to hold current and past values of W
logic [31:0] w[0:15];

// Create H registers to hold hash_block outputs
logic [31:0] h0, h1, h2, h3, h4, h5, h6, h7;

// Create registers to hold intermediate hash_block outputs
logic [31:0] a, b, c, d, e, f, g, h;

// Create logic to hold the w input for a hash op
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
        w_in = 32'd0; //pad with zeros
      end
    end
  end
  else begin //generate new values of w using previous values
    if (opcode == 2'b00) begin
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
    else if (opcode == 2'b01) begin
      w_in = left_rot_1(w[13] ^ w[8] ^ w[2] ^ w[0]);
    end
    else begin
      w_in = w[0] + (((w[1] >> 7) | (w[1] << 25)) ^ ((w[1] >> 18) | (w[1] << 14)) ^ (w[1] >> 3)) + w[9] + (((w[14] >> 17) | (w[14] << 15)) ^ ((w[14] >> 19) | (w[14] << 13)) ^ (w[14] >> 10));
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
          done <= 0;
          mem_we <= 0;
          mem_addr <= message_addr;
          rc <= 1;
          wc <= 0;
          state <= READ;
          t <= 0;
          if (opcode == 2'b00) begin
            h0 <= 32'h67452301;
            h1 <= 32'hefcdab89;
            h2 <= 32'h98badcfe;
            h3 <= 32'h10325476;
            h4 <= 32'd0;
            h5 <= 32'd0;
            h6 <= 32'd0;
            h7 <= 32'd0;
            a <= 32'h67452301;
            b <= 32'hefcdab89;
            c <= 32'h98badcfe;
            d <= 32'h10325476;
            e <= 32'd0;
            f <= 32'd0;
            g <= 32'd0;
            h <= 32'd0;
          end
          else if (opcode == 2'b01) begin
            h0 <= 32'h67452301;
            h1 <= 32'hefcdab89;
            h2 <= 32'h98badcfe;
            h3 <= 32'h10325476;
            h4 <= 32'hc3d2e1f0;
            h5 <= 32'd0;
            h6 <= 32'd0;
            h7 <= 32'd0;
            a <= 32'h67452301;
            b <= 32'hefcdab89;
            c <= 32'h98badcfe;
            d <= 32'h10325476;
            e <= 32'hc3d2e1f0;
            f <= 32'd0;
            g <= 32'd0;
            h <= 32'd0;
          end
          else begin
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
          end
        end
      end
      READ: begin
          state <= COMP;
          mem_addr <= message_addr + rc;
          rc <= rc + 1;
      end
      COMP: begin
      if ((t <= 63 && opcode != 2'b01) || (t <= 79 && opcode == 2'b01)) begin  //changed to 63 for md5
          state <= COMP;
          mem_we <= 0;
          mem_addr <= message_addr + rc;
          if (t <= 15) begin
            rc <= rc + 1;
          end
          if (t <= 15 || opcode != 2'b00) begin
            w[15] <= w_in;
            for(int i = 14; i >= 0; i--) begin
              w[i] <= w[i+1];
            end
          end
          t <= t + 1;
          if (opcode == 2'b00) begin
            {a, b, c, d} <= md5_hash_op(a, b, c, d, w_in, t);
          end
          else if (opcode == 2'b01) begin
            {a, b, c, d, e} <= sha1_hash_op(a, b, c, d, e, w_in, t);
          end
          else begin
            {a, b, c, d, e, f, g, h} <= sha256_hash_op(a, b, c, d, e, f, g, h, w_in, t);
          end
        end
        else begin
          t <= 0;
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
          4: begin
            if (opcode == 2'b00) begin
              mem_we <= 0;
              state <= IDLE;
              done <= 1;
            end
            else begin
              mem_addr <= output_addr + wc;
              mem_write_data <= h4;
              t <= 5;
              state <= WRITE;
            end
          end
          5: begin
            if (opcode == 2'b01) begin
              mem_we <= 0;
              state <= IDLE;
              done <= 1;
            end
            else begin
              mem_addr <= output_addr + wc;
              mem_write_data <= h5;
              t <= 6;
              state <= WRITE;
            end
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