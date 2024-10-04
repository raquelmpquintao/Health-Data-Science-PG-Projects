CALL `projeto`.`UP_CARGA_DIMENSAO_DATA`();
DELIMITER $$

CREATE PROCEDURE UP_CARGA_DIMENSAO_DATA()
BEGIN
    DECLARE ANO_INICIAL int;
	DECLARE ANO_FINAL int;
	DECLARE dtc_dia DATE;
	DECLARE dtr_data DATE;
    DECLARE dtc_dia2 varchar(255);
    DECLARE dia int;
    DECLARE quinzena int;
    DECLARE mes int;
    DECLARE ano int;
    DECLARE dtr_ano_mes int;
    DECLARE dtr_ano_mes2 varchar(255);
    DECLARE dtc_mes_ano varchar(255);
    DECLARE dtc_mes_ano_completo varchar(255);
    DECLARE dtc_mes_ano_numerico varchar(255);
    DECLARE nom_quinzena varchar(255);
    DECLARE nom_mes varchar(255);
    DECLARE nom_dia varchar(255);
    DECLARE num_bimestre int;
    DECLARE num_trimestre int;
    DECLARE num_quadrimestre int;
    DECLARE nom_trimestre varchar(255);
    DECLARE nom_bimestre varchar(255);
    DECLARE nom_quadrimestre varchar(255);
    DECLARE num_semestre int;
    DECLARE nom_semestre varchar(255);
    DECLARE sts_fim_de_semana varchar(255);
    DECLARE num_nivel int;
    DECLARE mes_ant int;
    DECLARE num_trimestre_ant int;
    DECLARE um_bimestre_ant int;
    DECLARE num_quadrimestre_ant int;
    DECLARE num_semestre_ant int;
    DECLARE ano_ant int;
    DECLARE num_dia_semana int;
    DECLARE ano_inicio int;
    DECLARE cont int;

	SET ano_inicio = 2020;
    SET ANO_FINAL = 2024;
    SET cont = 1;
    SET dtc_dia = STR_TO_DATE(CONCAT('01-01-', (CONVERT(ano_inicio,CHAR))), "%m-%d-%Y");
    
WHILE ano_inicio <= ANO_FINAL 
DO
    SET dia = DAY(dtc_dia);
    SET mes = MONTH(dtc_dia);
    SET ano = YEAR(dtc_dia);
    
	SET num_trimestre = EXTRACT(QUARTER FROM dtc_dia);      
    SET nom_trimestre = CONCAT(CONVERT(num_trimestre,CHAR), 'º Tri/', CONVERT(ano,CHAR));
    SET dtr_ano_mes = (ano*100) + mes;
    
    SET dtc_dia2 = CONVERT((dtr_ano_mes*100) + dia,CHAR);
    
    IF (mes <= 2) THEN
        SET num_bimestre = 1;
    ELSEIF (mes >2 and mes< 5) THEN
        SET num_bimestre = 2;
	ELSEIF (mes >=5 and mes< 7) THEN
        SET num_bimestre = 3;
    ELSEIF (mes >=7 and mes< 9) THEN
        SET num_bimestre = 4;
    ELSEIF (mes >=9 and mes<11) THEN
        SET num_bimestre = 5;
    ELSE  
        SET num_bimestre = 6;
    END IF;
    
    SET nom_bimestre = CONCAT(CONVERT(num_bimestre,CHAR), 'º Bim/', CONVERT(ano,CHAR));
    
	IF (num_bimestre <= 2) THEN
        SET num_quadrimestre = 1;
    ELSEIF (num_bimestre >2 and num_bimestre <5) THEN
        SET num_quadrimestre = 2;
	ELSE 
		SET num_quadrimestre = 3;
    END IF;
    
    SET nom_quadrimestre = CONCAT(CONVERT(num_quadrimestre,CHAR), 'º Quad/', CONVERT(ano,CHAR));
    
    IF (num_trimestre < 3) THEN
        SET num_semestre = 1;
    ELSE
        SET num_semestre = 2;
        END IF;
        
    SET nom_semestre = CONCAT(CONVERT(num_semestre,CHAR), 'º Sem/', CONVERT(ano,CHAR));

    SET num_dia_semana = WEEKDAY(dtc_dia);
    
    IF num_dia_semana > 4 THEN
        SET sts_fim_de_semana = 1;
    ELSE
        SET sts_fim_de_semana = 0;
    END IF;

	IF mes = 1 THEN SET nom_mes = 'Janeiro';
    ELSEIF mes = 2 THEN SET nom_mes = 'Fevereiro';
	ELSEIF mes = 3 THEN SET nom_mes = 'Março';
	ELSEIF mes = 4 THEN SET nom_mes = 'Abril';
	ELSEIF mes = 5 THEN SET nom_mes = 'Maio';
	ELSEIF mes = 6 THEN SET nom_mes = 'Junho';
	ELSEIF mes = 7 THEN SET nom_mes = 'Julho';
	ELSEIF mes = 8 THEN SET nom_mes = 'Agosto';
	ELSEIF mes = 9 THEN SET nom_mes = 'Setembro';
	ELSEIF mes = 10 THEN SET nom_mes = 'Outubro';
	ELSEIF mes = 11 THEN SET nom_mes = 'Novembro';
	ELSEIF mes = 12 THEN SET nom_mes = 'Dezembro';
	END IF;

   	IF (dia <16) THEN
        SET quinzena = 1;
    ELSE  
        SET quinzena = 2;
	END IF;
    
    SET nom_quinzena = CONCAT(CONVERT(quinzena,CHAR), 'º Quin/', CONVERT(SUBSTRING(nom_mes,1,3),CHAR));

	IF num_dia_semana = 6 THEN SET nom_dia = 'Domingo';
	ELSEIF num_dia_semana = 0 THEN SET nom_dia = 'Segunda';
	ELSEIF num_dia_semana = 1 THEN SET nom_dia = 'Terça';
	ELSEIF num_dia_semana = 2 THEN SET nom_dia = 'Quarta';
	ELSEIF num_dia_semana = 3 THEN SET nom_dia = 'Quinta';
	ELSEIF num_dia_semana = 4 THEN SET nom_dia = 'Sexta';
	ELSEIF num_dia_semana = 5 THEN SET nom_dia = 'Sábado';
	END IF;

   	IF (mes <10) THEN
       SET dtc_mes_ano_numerico = CONCAT('0', CONVERT(mes,CHAR), '/', SUBSTRING(CONVERT(dtr_ano_mes,CHAR),1,4));
    ELSE  
       SET dtc_mes_ano_numerico = CONCAT(CONVERT(mes,CHAR), '/', SUBSTRING(CONVERT(dtr_ano_mes,CHAR),1,4));
    END IF; 
    
   	SET dtc_mes_ano = CONCAT(SUBSTRING(nom_mes,1,3), '/', SUBSTRING(CONVERT(dtr_ano_mes,CHAR),1,4));
    SET dtc_mes_ano_completo = CONCAT(nom_mes, '/', SUBSTRING(CONVERT(dtr_ano_mes,CHAR),1,4));
    
	SET num_nivel = 1;
    SET dtr_ano_mes2 = CONVERT(dtr_ano_mes,CHAR);
    SET dtr_data= CONVERT(dtc_dia2,DATETIME);

    INSERT INTO DIM_DATA (DTC_DATA,NUM_ANO, NUM_DIA, NUM_MES, SK_DATA, DES_ANO_MES,DES_MES_ANO_NUMERICO,DES_MES_ANO_COMPLETO,DES_MES_ANO,DES_DATA_DIA,DES_DIA, DES_MES, DES_QUINZENA, DES_SEMESTRE, DES_TRIMESTRE, NUM_NIVEL, NUM_QUINZENA, NUM_SEMESTRE, NUM_TRIMESTRE, IND_FINAL_SEMANA, DES_BIMESTRE, DES_QUADRIMESTRE, NUM_BIMESTRE, NUM_QUADRIMESTRE)
    VALUES ( dtr_data,ano, dia, mes, cont,dtr_ano_mes2,dtc_mes_ano_numerico,dtc_mes_ano_completo,dtc_mes_ano,dtc_dia2,nom_dia, nom_mes, nom_quinzena, nom_semestre, nom_trimestre, num_nivel, quinzena, num_semestre, num_trimestre, sts_fim_de_semana, nom_bimestre, nom_quadrimestre, num_bimestre, num_quadrimestre);
   
    SET cont = cont + 1;
    
	IF (mes_ant != mes) THEN
        SET mes_ant = mes;
        SET num_nivel = 2;
    INSERT INTO DIM_DATA(DTC_DATA,NUM_ANO, NUM_DIA, NUM_MES, SK_DATA,DES_ANO_MES,DES_MES_ANO_NUMERICO,DES_MES_ANO_COMPLETO,DES_MES_ANO,DES_DATA_DIA,DES_DIA, DES_MES, DES_QUINZENA, DES_SEMESTRE, DES_TRIMESTRE, NUM_NIVEL, NUM_QUINZENA, NUM_SEMESTRE, NUM_TRIMESTRE, IND_FINAL_SEMANA, DES_BIMESTRE, DES_QUADRIMESTRE, NUM_BIMESTRE, NUM_QUADRIMESTRE)
        VALUES ( NULL,ano, NULL, mes, cont,dtr_ano_mes2,dtc_mes_ano_numerico,dtc_mes_ano_completo,dtc_mes_ano, NULL, NULL,nom_mes, NULL, nom_semestre, nom_trimestre, num_nivel, NULL, num_semestre, num_trimestre, 0, nom_bimestre, nom_quadrimestre, num_bimestre, num_quadrimestre);
        SET cont = cont + 1;
   END IF;

   SET dtc_dia = DATE_ADD(dtc_dia, INTERVAL 1 DAY);
   
   SET ano_inicio = YEAR(dtc_dia); 
	
END WHILE;

END$$

DELIMITER ;