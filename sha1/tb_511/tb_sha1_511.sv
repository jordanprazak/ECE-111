module tb_sha1();

logic           clk, reset_n, start;
logic   [ 31:0] message_addr, size, output_addr;
logic           done, mem_clk, mem_we;
logic   [ 15:0] mem_addr;
logic   [ 31:0] mem_write_data;
logic   [ 31:0] mem_read_data;

logic   [159:0] sha1_hash; // results here

logic   [ 31:0] dpsram[0:16383]; // each row has 32 bits
logic   [ 31:0] dpsram_tb[0:16383]; // for result testing, testbench only

logic   [ 31:0] message_seed = 32'h01234567; // modify message_seed to test your design

int             message_size = 511; // in bytes // change this number to test your design
int             pad_length;

int             t, m;
int             outloop;
int             cycles;

logic   [159:0] sha1_digest;

logic   [ 31:0] h0;
logic   [ 31:0] h1;
logic   [ 31:0] h2;
logic   [ 31:0] h3;
logic   [ 31:0] h4;

logic   [ 31:0] a, b, c, d, e;

logic   [ 31:0] w[0:79];

// instantiate your design
sha1 sha1_inst (clk, reset_n, start, message_addr, size, output_addr, done,
    mem_clk, mem_we, mem_addr, mem_write_data, mem_read_data);

// SHA1 f
function logic [31:0] sha1_f(input logic [7:0] t);
begin
   if (t <= 19)
       sha1_f = (b & c) | ((~b) & d);
   else if (t <= 39)
       sha1_f = b ^ c ^ d;
   else if (t <= 59)
       sha1_f = (b & c) | (b & d) | (c & d);
   else
       sha1_f = b ^ c ^ d;
end
endfunction

// SHA1 k
function logic [31:0] sha1_k(input logic [7:0] t);
begin
   if (t <= 19)
       sha1_k = 32'h5a827999;
   else if (t <= 39)
       sha1_k = 32'h6ed9eba1;
   else if (t <= 59)
       sha1_k = 32'h8f1bbcdc;
   else
       sha1_k = 32'hca62c1d6;
end
endfunction

// SHA1 hash round
function logic [159:0] sha1_op(input logic [31:0] a, b, c, d, e, w,
                               input logic [7:0] t);
   logic [31:0] temp, tc; // internal signals
begin
   temp = ((a << 5)|(a >> 27)) + sha1_f(t) + e + sha1_k(t) + w;
   tc = ((b << 30)|(b >> 2));
   sha1_op = {temp, a, tc, c, d};
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
    h4 = 32'hC3D2E1F0;

    // COMPUTE SHA1 HASH

    for (m = 0; m < outloop; m = m + 1) begin
        // W ARRAY EXPANSION

        for (t = 0; t < 80; t = t + 1) begin
            if (t < 16) begin
                w[t] = dpsram_tb[t+m*16];
            end else begin
                w[t] = w[t-3] ^ w[t-8] ^ w[t-14] ^ w[t-16];
                w[t] = (w[t] << 1) | (w[t] >> 31);
            end
        end

        // INITIAL HASH AT ROUND K

        a = h0;
        b = h1;
        c = h2;
        d = h3;
        e = h4;

        // HASH ROUNDS

        for (t = 0; t < 80; t = t + 1) begin
            {a, b, c, d, e} = sha1_op(a, b, c, d, e, w[t], t);
        end

        // FINAL HASH

        h0 = h0 + a;
        h1 = h1 + b;
        h2 = h2 + c;
        h3 = h3 + d;
        h4 = h4 + e;
    end

    sha1_digest = {h0, h1, h2, h3, h4};

    // WAIT UNTIL ENTIRE FRAME IS HASHED, THEN DISPLAY HASH RESULT

    wait (done == 1);

    // DISPLAY HASH RESULT

    $display("-----------------------\n");
    $display("correct hash result is:\n");
    $display("-----------------------\n");
    $display("%x\n", sha1_digest);

    sha1_hash = {
        dpsram[output_addr],
        dpsram[output_addr+1],
        dpsram[output_addr+2],
        dpsram[output_addr+3],
        dpsram[output_addr+4]
    };

    $display("-----------------------\n");
    $display("Your result is:        \n");
    $display("-----------------------\n");
    $display("%x\n", sha1_hash);

    $display("***************************\n");

    if (sha1_digest == sha1_hash) begin
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