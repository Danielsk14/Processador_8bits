
--DANIEL MARTINS DE CARVALHO, 2321386
--FILIPE AUGUSTO PARREIRA ALMEIDA, 2320622
--GUSTAVO GEOVANE TAMIÃO DE SOUZA, 2271990
--IAGO MACARINI BRITO, 2320665
--JOÃO VITOR GARCIA CARVALHO, 2270340
--MICHAEL PARIZ PEREIRA, 2321653
--PEDRO HENRIQUE TEIXEIRA, 2270390
--SANDRO PINHEIRO CHRISTE, 2270404


library IEEE;
use IEEE.STD_LOGIC_1164.all;
use ieee.numeric_std.all;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

--memoria de instruçoes
entity Processador is
port(
  clk, clr, we, wer: in STD_LOGIC;
  DEC: out std_logic_vector(6 downto 0); -- variavel que vai guardar o resultado dos display que serão acendidos.
  INST: out std_logic_vector(6 downto 0);
  INST2: out std_logic_vector(6 downto 0)
);
end Processador;
architecture logic of Processador is


  signal end_inst : STD_LOGIC_VECTOR (3 downto 0) := "0000"; -- saida do contador e entrada da memoria de instuçoes  
  signal op : STD_LOGIC_VECTOR (3 downto 0); -- operaçao da mem instruçao
  signal end_val : STD_LOGIC_VECTOR (3 downto 0); -- endereço saida da memoria de instruçao
    
  signal ula_op  : STD_LOGIC_VECTOR (3 downto 0);--código de operação que vai ser mandado pra ula
  signal ula_end : STD_LOGIC_VECTOR (3 downto 0);--endereço em que o resultado vai ser salvo
  
  signal	A,B: std_logic_vector(3 downto 0); -- variáveis de entrada para as operações.
  signal operacao: std_logic_vector(3 downto 0); -- sinal de entrada para saber qual será a operação para ser usada no multiplexador
  signal D: std_logic_vector(3 downto 0); -- sinal usado para o multiplexador e usado para compração com o decodificador.
  signal x: std_logic;  -- só usado para diferenciar as operações.
  signal Y: boolean;  -- sinal usado para o multiplexador e comparação para o decodificador também
 
  --memoria de instruçoes
type Mem_Inst_Set is array (0 to 15) of STD_LOGIC_VECTOR (7 downto 0);
  constant mem : Mem_Inst_Set := (
  "11000101",
  "00000010",
  "00010100",
  "00100101",
  "00110001",
  "10100100",
  "01000110",
  "01010010",
  "01101010",
  "01111011",
  "10000110",
  "10010101",
  "10110000",
  "11001010",
  "11010110",
  "00011001" );
  
type array_mem is array(0 to 15) of STD_LOGIC_VECTOR(7 downto 0);--declarando o array de memoria 
	signal un_mem : array_mem := ("01100111","01010100","01100010","01110001","00010100","00000101","01010000","01100100","01000110","00100101","00110100","01100001","10100010",	"01100100","01000101","10011011");

type Mem_res_Set is array (0 to 4) of STD_LOGIC_VECTOR (7 downto 0);
	signal storage: Mem_Inst_set ;
  
begin 

process (clk, clr)

begin
   --PARTE DO CONTADOR
	
   if (clr = '1') then
	   end_inst <= "0000";
   elsif (clk'event and clk = '1') then  
	   end_inst <= end_inst + 1;
   end if;
   
end process;
	
	--PARTE DA MEMÓRIA DE INSTRUÇAO
	
	--passa os valores do array para as saidas da mem_int
  op <= mem (to_integer(unsigned(end_inst))) (7 downto 4); 
  end_val <= mem (to_integer(unsigned(end_inst))) (3 downto 0);

  
   --PARTE DA UNIDADE DE CONTROLE
process(op, end_val)
begin
	
  	--RECEBE A INSTRUÇÃO DA MEMÓRIA, E GERA UMA SAIDA DE MESMO VALOR PARA A ULA
     ula_op <= op;
					 
						 
	-- RECEBE A INSTRUÇÃO DA MEMÓRIA E ENVIA O ENDEREÇO EM QUE O RESULTADO DEVE SER SALVO PARA A ULA					 
     ula_end <= end_val;		

		--CASO A OPERAÇÃO SEJA UM JUMP	
  if op = "1010" then
    ula_op <= mem (to_integer(unsigned(end_val))) (7 downto 4); 
	 ula_end <= mem (to_integer(unsigned(end_val))) (3 downto 0); 
  end if;
  
  
  case(op) is      -- case do decodificador.
				when "0000" => INST <= "0001000"; --'A' SOMA
				when "0001" => INST <= "0010010"; --'5' SUB
				when "0010" => INST <= "0110000"; --'3' MULTI
				when "0011" => INST <= "1000000"; --'D' DIVI
				when "0100" => INST <= "0110000"; --'3' MAIOR
				when "0101" => INST <= "0110000"; --'3' MENOR
 				when "0110" => INST <= "1111001"; --'1' IGUAL
				when "0111" => INST <= "0001000"; --'A' MAIOR IGUAL
				when "1000" => INST <= "0110000"; --'3' MENOR IGUAL
				when "1001" => INST <= "1000000"; --'0' DIFERENTE
				when "1010" => INST <= "0000010"; --'6'JUMP
				when "1011" => INST <= "0110000"; --'3'MOV
				when "1100" => INST <= "1111000"; --'7'LOAD
				when "1101" => INST <= "0010010"; --'5'STORE
				when others => INST <= "1111111";
			end case;
				
  case(op) is      -- case do decodificador.
				when "0000" => INST2 <= "1000000"; --'0' SOMA
				when "0001" => INST2 <= "0000000"; --'B' SUB
				when "0010" => INST2 <= "1000110"; --'C' MULTI
				when "0011" => INST2 <= "1111001"; --'1' DIVI
				when "0100" => INST2 <= "0001000"; --'A' MAIOR
				when "0101" => INST2 <= "0000110"; --'E' MENOR
				when "0110" => INST2 <= "0000010"; --'6' IGUAL
				when "0111" => INST2 <= "1111001"; --'1' MAIOR IGUAL
				when "1000" => INST2 <= "1111001"; --'1'MENOR IGUAL
				when "1001" => INST2 <= "0001110"; --'F'DIFERENTE
				when "1010" => INST2 <= "1000110"; --'C'JUMP
				when "1011" => INST2 <= "1000000"; --'0'MOV
				when "1100" => INST2 <= "1000000"; --'0' LOAD
				when "1101" => INST2 <= "1111000"; --'7' STORE
				when others => INST2 <= "1111111";
			end case;
  
end process;
 
 
  --PARTE DA ULA
  
    process(A,B,operacao,ula_op,ula_end,we,clk,un_mem) -- processo do nosso case de multiplexador(operações).
	 begin
	 operacao <= ula_op;
		
		case(operacao) is
		   when "0000" =>
				if (we = '1') then
			     A <= un_mem (to_integer(unsigned(ula_end))) (7 downto 4); 
	           B <= un_mem (to_integer(unsigned(ula_end))) (3 downto 0); 
				end if;
		   	D <=	A+B; -- operação de soma
				if(wer = '0') then
					storage(0)<= std_logic_vector(resize(unsigned(D),8));				
				end if;
				x <= '1';
		   when "0001" =>
			   if (we = '1') then
			     A <= un_mem (to_integer(unsigned(ula_end))) (7 downto 4); 
	           B <= un_mem (to_integer(unsigned(ula_end))) (3 downto 0); 
				end if;
		   	D <= A-B; -- operação de subtração
				if(wer = '0') then
					storage(1)<= std_logic_vector(resize(unsigned(D),8));				
				end if;
				x <= '1';
		   when "0010" => 
			   if (we = '1') then
			     A <= un_mem (to_integer(unsigned(ula_end))) (7 downto 4); 
	           B <= un_mem (to_integer(unsigned(ula_end))) (3 downto 0); 
				end if;
			   D <= std_logic_vector(to_unsigned(to_integer(unsigned(A)) * to_integer(unsigned(B)), 4)); -- operação de mult.
				if(wer = '0') then
					storage(2)<= std_logic_vector(resize(unsigned(D),8));				
				end if;
														--conversor de varios tipos
														--to_integer(unsigned(X))
														--std_logic_vector(to_unsigned(K, N))
			   x <= '1';
		   when "0011" =>
			   if (we = '1') then
			     A <= un_mem (to_integer(unsigned(ula_end))) (7 downto 4); 
	           B <= un_mem (to_integer(unsigned(ula_end))) (3 downto 0); 
				end if;
		     	D <= std_logic_vector(to_unsigned(to_integer(unsigned(a)) / to_integer(unsigned(b)), 4)); -- operação de divis.
				if(wer = '0') then
					storage(3)<= std_logic_vector(resize(unsigned(D),8));				
				end if;
				x <= '1';
		   when "0110" =>
			   if (we = '1') then
			     A <= un_mem (to_integer(unsigned(ula_end))) (7 downto 4); 
	           B <= un_mem (to_integer(unsigned(ula_end))) (3 downto 0); 
				end if;
		   	Y <= A = B; -- operação de igualdade.			 
				x <= '0';
		   when "0100" =>
			   if (we = '1') then
			     A <= un_mem (to_integer(unsigned(ula_end))) (7 downto 4); 
	           B <= un_mem (to_integer(unsigned(ula_end))) (3 downto 0); 
				end if;
		    	Y <= A > B; -- operação de maior.
				x <= '0';
		   when "0101" => 
			   if (we = '1') then
			     A <= un_mem (to_integer(unsigned(ula_end))) (7 downto 4); 
	           B <= un_mem (to_integer(unsigned(ula_end))) (3 downto 0); 
				end if;
			   Y <= A < B; -- operação de menor.
				x <= '0';
			when "0111" =>
			   if (we = '1') then
			     A <= un_mem (to_integer(unsigned(ula_end))) (7 downto 4); 
	           B <= un_mem (to_integer(unsigned(ula_end))) (3 downto 0); 
				end if;
		    	Y <= A >= B; --maior ou igual
				x <= '0';
			when "1000" => 
			   if (we = '1') then
			     A <= un_mem (to_integer(unsigned(ula_end))) (7 downto 4); 
	           B <= un_mem (to_integer(unsigned(ula_end))) (3 downto 0); 
				end if;
			   Y <= A <= B; --menor ou igual
				x <= '0';
			when "1001" =>
			   if (we = '1') then
			     A <= un_mem (to_integer(unsigned(ula_end))) (7 downto 4); 
	           B <= un_mem (to_integer(unsigned(ula_end))) (3 downto 0); 
				end if;
		    	Y <= A /= B; -- diferente
				x <= '0';
			when "1011" =>
			   if (we = '1') then
			     A <= un_mem (to_integer(unsigned(ula_end))) (7 downto 4); 
	           B <= un_mem (to_integer(unsigned(ula_end))) (3 downto 0); 
				end if;
			   A <= B; -- MOV
				Y <= true;
				x <= '0';
			when "1100" => --load
			   if (we = '1') then
			     A <= un_mem (to_integer(unsigned(ula_end))) (7 downto 4); 
	           B <= un_mem (to_integer(unsigned(ula_end))) (3 downto 0); 
				end if;
				Y <= true;
				x <= '0';
			when "1101" =>
			   if(wer = '0') then
					storage(4) <= std_logic_vector(resize(unsigned(A),8));				
				end if;
				Y <= true;
				x <= '0';
			
		when others =>
			if (we = '1') then
		     A <= un_mem (to_integer(unsigned(ula_end))) (7 downto 4); 
	        B <= un_mem (to_integer(unsigned(ula_end))) (3 downto 0); 
			end if;
	     	D <= A+B; -- qualquer outro caso ele faz a soma.
			x <= '1';
		end case;
	end process;
	 
		process(D,x, Y)       -- processo referente ao case do decodificador.
		begin 
		if(x = '1') then
			case(D) is      -- case do decodificador.
				when "0000" => DEC <= "1000000"; --'0'
				when "0001" => DEC <= "1111001"; --'1'
				when "0010" => DEC <= "0100100"; --'2'
				when "0011" => DEC <= "0110000"; --'3'
				when "0100" => DEC <= "0011001"; --'4'
				when "0101" => DEC <= "0010010"; --'5'
				when "0110" => DEC <= "0000010"; --'6'
				when "0111" => DEC <= "1111000"; --'7'
				when "1000" => DEC <= "0000000"; --'8'
				when "1001" => DEC <= "0010000"; --'9'
				when "1010" => DEC <= "0001000"; --'A'
				when "1011" => DEC <= "0000011"; --'B'
				when "1100" => DEC <= "1000110"; --'C'
				when "1101" => DEC <= "1000000"; --'D'
				when "1110" => DEC <= "0000110"; --'E'
				when "1111" => DEC <= "0001110"; --'F'
				when others => DEC <= "1111111";
				end case;
		else 
			case(Y) is 
				when false =>  DEC <= "1000000"; --'0'
				when true  =>  DEC <= "1111001"; --'1'
				when others => DEC <= "1000000";
				end case;
				
		end if;
  
  
	
end process;


end logic;