module hash_block(input logic [31:0] a, b, c, d, e, w,
                  input logic [7:0] t,
                 output logic [159:0] hash);

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

  assign hash = hash_op(a, b, c, d, e, w, t);
endmodule