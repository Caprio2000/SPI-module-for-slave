----------------------------------------------Engineer:Caprio Mistry
----------------------------------------------Organization: Optmized solutions limited

LIBRARY ieee;
USE ieee.std_logic_1164.all;
USE ieee.std_logic_arith.all;

ENTITY spi_slave IS
  GENERIC(
    cpol    : STD_LOGIC := '0';  --polarity
    cpha    : STD_LOGIC := '0';  --phase
    d_width : INTEGER := 8);     --width
  PORT(
    sclk         : IN     STD_LOGIC;  --clck from master
    reset_n      : IN     STD_LOGIC;  --reset 
    ss_n         : IN     STD_LOGIC;  -- chip select -slave
    mosi         : IN     STD_LOGIC;  --master out, slave in (i/o for slave)
    rx_req       : IN     STD_LOGIC;  -- transaction
    st_load_en   : IN     STD_LOGIC;  -- enabled
    st_load_trdy : IN     STD_LOGIC;  -- trdy i/o
    st_load_rrdy : IN     STD_LOGIC;  --rrdy i/o
    st_load_roe  : IN     STD_LOGIC;  --roe i/o
    tx_load_en   : IN     STD_LOGIC;  --async buffer load enabled
    tx_load_data : IN     STD_LOGIC_VECTOR(d_width-1 DOWNTO 0);  --async tx data load
    trdy         : BUFFER STD_LOGIC := '0';  --transmit ready bit
    rrdy         : BUFFER STD_LOGIC := '0';  --receive ready bit
    roe          : BUFFER STD_LOGIC := '0';  -- error bit
    rx_data      : OUT    STD_LOGIC_VECTOR(d_width-1 DOWNTO 0) := (OTHERS => '0');  --receive register output 
    busy         : OUT    STD_LOGIC := '0';  --busy signal to check SPI
    miso         : OUT    STD_LOGIC := 'Z'); --master in, slave out (o/u for slave)------- (i/o for master)
END spi_slave;

ARCHITECTURE logic OF spi_slave IS
  SIGNAL mode    : STD_LOGIC;  -- clock polarity mode
  SIGNAL clk     : STD_LOGIC;  --clock
  SIGNAL bit_cnt : STD_LOGIC_VECTOR(d_width+8 DOWNTO 0);  -- communication bit
  SIGNAL wr_add  : STD_LOGIC;  --address of register W
  SIGNAL rd_add  : STD_LOGIC;  --address of register R
  SIGNAL rx_buf  : STD_LOGIC_VECTOR(d_width-1 DOWNTO 0) := (OTHERS => '0');  --receiver buffer
  SIGNAL tx_buf  : STD_LOGIC_VECTOR(d_width-1 DOWNTO 0) := (OTHERS => '0');  --transmit buffer
BEGIN
  busy <= NOT ss_n;  --high when SPI engaging
  
  --clock adjustment  writes are on rising edge and reads on falling edge
  mode <= cpol XOR cpha;  -- write on rising edge
  WITH mode SELECT
    clk <= sclk WHEN '1',
           NOT sclk WHEN OTHERS;

  --keep track of miso/mosi bit counts for data alignmnet
  PROCESS(ss_n, clk)
  BEGIN
    IF(ss_n = '1' OR reset_n = '0') THEN                         --this slave is not selected or being reset
     bit_cnt <= (conv_integer(NOT cpha) => '1', OTHERS => '0'); --reset miso/mosi bit count
    ELSE                                                         --this slave is selected
      IF(rising_edge(clk)) THEN                                  --new bit on miso/mosi
        bit_cnt <= bit_cnt(d_width+8-1 DOWNTO 0) & '0';          --shift active bit indicator
      END IF;
    END IF;
  END PROCESS;

  PROCESS(ss_n, clk, st_load_en, tx_load_en, rx_req)
  BEGIN
  
    --write address register ('0' for receive, '1' for status)
    IF(bit_cnt(1) = '1' AND falling_edge(clk)) THEN
      wr_add <= mosi;
    END IF;

    --read address register ('0' for transmit, '1' for status)
    IF(bit_cnt(2) = '1' AND falling_edge(clk)) THEN
      rd_add <= mosi;
    END IF;
    
    --trdy register
    IF((ss_n = '1' AND st_load_en = '1' AND st_load_trdy = '0') OR reset_n = '0') THEN  
      trdy <= '0';   --cleared by user logic or reset
    ELSIF(ss_n = '1' AND ((st_load_en = '1' AND st_load_trdy = '1') OR tx_load_en = '1')) THEN
      trdy <= '1';   --set when tx buffer written or set by user logic
    ELSIF(falling_edge(clk)) THEN
      IF(wr_add = '1' AND bit_cnt(9) = '1') THEN
        trdy <= mosi;  --new value written over spi bus
      ELSIF(rd_add = '0' AND bit_cnt(d_width+8) = '1') THEN
        trdy <= '0';   --clear when transmit buffer read
      END IF;
    END IF;
    
    --rrdy register
    IF((ss_n = '1' AND ((st_load_en = '1' AND st_load_rrdy = '0') OR rx_req = '1')) OR reset_n = '0') THEN
      rrdy <= '0';   --cleared by user logic or rx_data has been requested or reset
    ELSIF(ss_n = '1' AND st_load_en = '1' AND st_load_rrdy = '1') THEN
      rrdy <= '1';   --set when set by user logic
    ELSIF(falling_edge(clk)) THEN
      IF(wr_add = '1' AND bit_cnt(10) = '1') THEN
        rrdy <= mosi;  --new value written over spi bus
      ELSIF(wr_add = '0' AND bit_cnt(d_width+8) = '1') THEN
        rrdy <= '1';   --set when new data received
      END IF;
    END IF;
    
    --roe register
    IF((ss_n = '1' AND st_load_en = '1' AND st_load_roe = '0') OR reset_n = '0') THEN
      roe <= '0';   --cleared by user logic or reset
    ELSIF(ss_n = '1' AND st_load_en = '1' AND st_load_roe = '1') THEN
      roe <= '1';   --set by user logic
    ELSIF(falling_edge(clk)) THEN
      IF(rrdy = '1' AND wr_add = '0' AND bit_cnt(d_width+8) = '1') THEN
        roe <= '1';   --set by actual overrun
      ELSIF(wr_add = '1' AND bit_cnt(11) = '1') THEN
        roe <= mosi;  --new value written by spi bus
      END IF;
    END IF;
    
    --receive registers
    --write to the receive register from master
    IF(reset_n = '0') THEN
      rx_buf <= (OTHERS => '0');
    ELSE
      FOR i IN 0 TO d_width-1 LOOP          
        IF(wr_add = '0' AND bit_cnt(i+9) = '1' AND falling_edge(clk)) THEN
          rx_buf(d_width-1-i) <= mosi;
        END IF;
      END LOOP;
    END IF;
    --fulfill user logic request for receive data
    IF(reset_n = '0') THEN
      rx_data <= (OTHERS => '0');
    ELSIF(ss_n = '1' AND rx_req = '1') THEN  
      rx_data <= rx_buf;
    END IF;

    --transmit registers
    IF(reset_n = '0') THEN
      tx_buf <= (OTHERS => '0');
    ELSIF(ss_n = '1' AND tx_load_en = '1') THEN  --load transmit register from user logic
      tx_buf <= tx_load_data;
    ELSIF(rd_add = '0' AND bit_cnt(7 DOWNTO 0) = "00000000" AND bit_cnt(d_width+8) = '0' AND rising_edge(clk)) THEN
      tx_buf(d_width-1 DOWNTO 0) <= tx_buf(d_width-2 DOWNTO 0) & tx_buf(d_width-1);  --shift through tx data
    END IF;

    --miso output register
    IF(ss_n = '1' OR reset_n = '0') THEN           --no transaction occuring or reset
      miso <= 'Z';
    ELSIF(rising_edge(clk)) THEN
      IF(rd_add = '1') THEN  --write status register to master
        CASE bit_cnt(10 DOWNTO 8) IS
          WHEN "001" => miso <= trdy;
          WHEN "010" => miso <= rrdy;
          WHEN "100" => miso <= roe;
          WHEN OTHERS => NULL;
        END CASE;
      ELSIF(rd_add = '0' AND bit_cnt(7 DOWNTO 0) = "00000000" AND bit_cnt(d_width+8) = '0') THEN
        miso <= tx_buf(d_width-1);                  --send transmit register data to master
      END IF;
    END IF;
    
  END PROCESS;
END logic;
