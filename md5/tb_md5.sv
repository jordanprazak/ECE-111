module tb_md5();

logic           clk, reset_n, start;
logic   [ 31:0] message_addr, size, output_addr;
logic           done, mem_clk, mem_we;
logic   [ 15:0] mem_addr;
logic   [ 31:0] mem_write_data;
logic   [ 31:0] mem_read_data;

logic   [127:0] md5_hash; // results here

logic   [ 31:0] dpsram[0:16383]; // each row has 32 bits
logic   [ 31:0] dpsram_tb[0:16383]; // for result testing, testbench only

logic   [ 31:0] message_seed = 32'h01234567; // modify message_seed to test your design

int             message_size = 120; // in bytes // change this number to test your design
int             pad_length;

int             t, m;
int             outloop;
int             cycles;

logic   [127:0] md5_digest;

logic   [ 31:0] h0;
logic   [ 31:0] h1;
logic   [ 31:0] h2;
logic   [ 31:0] h3;

logic   [ 31:0] a, b, c, d;

logic   [ 31:0] w[0:63];

// instantiate your design
md5 md5_inst (clk, reset_n, start, message_addr, size, output_addr, done,
    mem_clk, mem_we, mem_addr, mem_write_data, mem_read_data);

// MD5 S constants
parameter byte S[0:63] = '{
    8'd7, 8'd12, 8'd17, 8'd22, 8'd7, 8'd12, 8'd17, 8'd22, 8'd7, 8'd12, 8'd17, 8'd22, 8'd7, 8'd12, 8'd17, 8'd22,
    8'd5, 8'd9,  8'd14, 8'd20, 8'd5, 8'd9,  8'd14, 8'd20, 8'd5, 8'd9,  8'd14, 8'd20, 8'd5, 8'd9,  8'd14, 8'd20,
    8'd4, 8'd11, 8'd16, 8'd23, 8'd4, 8'd11, 8'd16, 8'd23, 8'd4, 8'd11, 8'd16, 8'd23, 8'd4, 8'd11, 8'd16, 8'd23,
    8'd6, 8'd10, 8'd15, 8'd21, 8'd6, 8'd10, 8'd15, 8'd21, 8'd6, 8'd10, 8'd15, 8'd21, 8'd6, 8'd10, 8'd15, 8'd21
};

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

// MD5 g
function logic[3:0] md5_g(input logic [7:0] t);
begin
   if (t <= 15)
       md5_g = t;
   else if (t <= 31)
       md5_g = (5*t + 1) % 16;
   else if (t <= 47)
       md5_g = (3*t + 5) % 16;
   else
       md5_g = (7*t) % 16;
end
endfunction

// MD5 f
function logic[31:0] md5_f(input logic [7:0] t);
begin
    if (t <= 15)
        md5_f = (b & c) | ((~b) & d);
    else if (t <= 31)
        md5_f = (d & b) | ((~d) & c);
    else if (t <= 47)
        md5_f = b ^ c ^ d;
    else
        md5_f = c ^ (b | (~d));
end
endfunction

// MD5 hash round
function logic[127:0] md5_op(input logic [31:0] a, b, c, d, w,
                             input logic [7:0] t);
    logic [31:0] t1, t2; // internal signals
begin
    t1 = a + md5_f(t) + md5_k[t] + w;
    t2 = b + ((t1 << S[t])|(t1 >> (32-S[t])));
    md5_op = {d, t2, b, c};
end
endfunction

// clock generator
always begin
    #10;
    clk = 1'b1;
    #10
    clk = 1'b0;
end

// main testbench
initial
begin
    // RESET HASH CO-PROCESSOR

    @(posedge clk) reset_n = 0;
    for (m = 0; m < 2; m = m + 1) @(posedge clk);
    reset_n = 1;
    for (m = 0; m < 2; m = m + 1) @(posedge clk);

    // SET MESSAGE LOCATION

    size = message_size;
    message_addr = 32'd0;
    output_addr = message_addr + ((message_size-1))/4 + 1;

    // CREATE AND DISPLAY MESSAGETEXT

    $display("-----------\n");
    $display("Messagetext\n");
    $display("-----------\n");

    dpsram[message_addr+0] = message_seed;
    dpsram_tb[0] = message_seed;

    $display("%x\n", dpsram[0]);

    for (m = 1; m < (message_size-1)/4+1; m = m + 1) begin // data generation
        dpsram[message_addr+m] = (dpsram[message_addr+m-1]<<1)|(dpsram[message_addr+m-1]>>31);
        dpsram_tb[m] = dpsram[message_addr+m];
        $display("%x\n", dpsram[message_addr+m]);
    end

    // START PROCESSOR

    start = 1'b1;
    for (m = 0; m < 2; m = m + 1) @(posedge clk);
    start = 1'b0;

    // PERFORM PADDING OF MESSAGE

    // calculate total number of bytes after padding (before appending total length)
    if ((message_size + 1) % 64 <= 56 && (message_size + 1) % 64 > 0)
        pad_length = (message_size/64)*64 + 56;
    else
        pad_length = (message_size/64+1)*64 + 56;

    case (message_size % 4) // pad bit 1
    0: dpsram_tb[message_size/4] = 32'h80000000;
    1: dpsram_tb[message_size/4] = dpsram_tb[message_size/4] & 32'h FF000000 | 32'h 00800000;
    2: dpsram_tb[message_size/4] = dpsram_tb[message_size/4] & 32'h FFFF0000 | 32'h 00008000;
    3: dpsram_tb[message_size/4] = dpsram_tb[message_size/4] & 32'h FFFFFF00 | 32'h 00000080;
    endcase

    for (m = message_size/4+1; m < pad_length/4; m = m + 1) begin
        dpsram_tb[m] = 32'h00000000;
    end

    dpsram_tb[pad_length/4] = message_size >> 29; // append length of message in bits (before pre-processing)
    dpsram_tb[pad_length/4+1] = message_size * 8;
    pad_length = pad_length + 8; // final length after pre-processing

    outloop = pad_length/64; // break message into 512-bit chunks (64 bytes)

    // SET INITIAL HASH

    h0 = 32'h67452301;
    h1 = 32'hEFCDAB89;
    h2 = 32'h98BADCFE;
    h3 = 32'h10325476;

    // COMPUTE MD5 HASH

    for (m = 0; m < outloop; m = m + 1) begin
        // W ARRAY EXPANSION

        for (t = 0; t < 64; t = t + 1) begin
            if (t < 16) begin
                w[t] = dpsram_tb[t+m*16];
            end else begin
                w[t] = w[md5_g(t)];
            end
        end

        // INITIAL HASH AT ROUND K

        a = h0;
        b = h1;
        c = h2;
        d = h3;

        // HASH ROUNDS

        for (t = 0; t < 64; t = t + 1) begin
            {a, b, c, d} = md5_op(a, b, c, d, w[t], t);
        end

        // FINAL HASH

        h0 = h0 + a;
        h1 = h1 + b;
        h2 = h2 + c;
        h3 = h3 + d;
    end

    md5_digest = {h0, h1, h2, h3};

    // WAIT UNTIL ENTIRE FRAME IS HASHED, THEN DISPLAY HASH RESULT

    wait (done == 1);

    // DISPLAY HASH RESULT

    $display("-----------------------\n");
    $display("correct hash result is:\n");
    $display("-----------------------\n");
    $display("%x\n", md5_digest);

    md5_hash = {
        dpsram[output_addr],
        dpsram[output_addr+1],
        dpsram[output_addr+2],
        dpsram[output_addr+3]
    };

    $display("-----------------------\n");
    $display("Your result is:        \n");
    $display("-----------------------\n");
    $display("%x\n", md5_hash);

    $display("***************************\n");

    if (md5_digest == md5_hash) begin
        $display("Congratulations! You have the correct hash result!\n");
        $display("Total number of cycles: %d\n\n", cycles);
    end else begin
        $display("Error! The hash result is wrong!\n");
    end

    $display("***************************\n");

    $stop;
end

// memory model
always @(posedge mem_clk)
begin
    if (mem_we) // write
        dpsram[mem_addr] = mem_write_data;
    else // read
        mem_read_data = dpsram[mem_addr];
end

// track # of cycles
always @(posedge clk)
begin
    if (!reset_n)
        cycles = 0;
    else
        cycles = cycles + 1;
end

endmodule