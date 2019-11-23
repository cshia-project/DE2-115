library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
library CSHIA;
use CSHIA.apuf_pkg.all;

entity apuf_unit is
	-- The number of stages has to be at least 2 and multiple of 2 .
	generic (
		n_stages 				: integer := 16
	);
	port (
		in1, in2				: in std_logic;
		challenge				: in std_logic_vector (n_stages-1 downto 0);
		out1, out2				: out std_logic
	);
end entity;

architecture RTL of apuf_unit is
	attribute keep : STRING;
	
	signal v1, v2 : STD_LOGIC_VECTOR (n_stages/2 downto 0);	
	
	attribute keep of v1, v2 : signal is "true";
begin
	assert ((n_stages >= 2) or (n_stages mod 2 = 0))
	report "APUF stages has to be greater than 2 and multiple of 2." severity failure;

	apuf0: apuf_stage
	port map (
		in1		=> in1,
		in2		=> in2,
		path	=> challenge(1 downto 0),
		out1	=> v1(0),
		out2	=> v2(0)
	);
	apufs:
	for i in 1 to n_stages/2-1 generate
		apuf_middle: apuf_stage
		port map (
			in1		=> v1(i-1),
			in2		=> v2(i-1),
			path	=> challenge((i+1)*2-1 downto i*2),
			out1	=> v1(i),
			out2	=> v2(i)
		);
	end generate;

	out1 <= v1(n_stages/2-1);
	out2 <= v2(n_stages/2-1);
end architecture;

