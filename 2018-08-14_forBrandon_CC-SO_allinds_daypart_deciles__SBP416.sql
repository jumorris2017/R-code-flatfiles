/*query for CC score and survey counts by daypart by store */
/*CAW*/
WITH sq AS
(SELECT DISTINCT
  TO_CHAR(sr.TRANS_DTM, 'HH24') AS HOUR
    ,(CASE WHEN TO_CHAR(sr.TRANS_DTM, 'HH24') >=7 AND TO_CHAR(sr.TRANS_DTM, 'HH24') <= 11 THEN '1_am' -- midday
        WHEN TO_CHAR(sr.TRANS_DTM, 'HH24') >=14 AND TO_CHAR(sr.TRANS_DTM, 'HH24') <= 17 THEN '2_pm' -- pm
        ELSE 'NA' 
        END) AS DAY_PART
  ,(CASE WHEN ca.DAY_IN_CAL_WK_NUM BETWEEN 2 AND 6 THEN '1_mf' 
        WHEN ca.DAY_IN_CAL_WK_NUM in (1,7) THEN '2_wknd' 
        ELSE 'NA' 
        END) AS WEEK_PART
  ,sr.QSTN_ID
  ,SUM(CASE  WHEN sr.RSPNS_ID = '5' AND sr.QSTN_ID = 'Q1' THEN COALESCE(w.WEIGHT_RT,1) /* coalesce returns non-null weight value, OR if null, returns 1 */
    WHEN sr.RSPNS_ID = '7' THEN COALESCE(w.WEIGHT_RT,1) ELSE 0 END) AS TOTAL_TB
  ,SUM(COALESCE(w.WEIGHT_RT,1)) AS TOTAL_RSPNS
  ,ca.FSCL_YR_NUM
  ,sr.STORE_NUM

FROM APPOTHER.AFT_CV_SRVY_RSPNS sr

JOIN APPCA.D_CAL ca
  ON ca.CAL_DT = TRUNC(sr.TRANS_DTM)
  
JOIN APPCA.D_STORE_VERS st
  ON sr.STORE_NUM = st.STORE_NUM
    AND st.CURRENT_FLG = 'Y'
    AND st.OWNR_TYPE_CD = 'CO'
    AND st.CNTRY_CD_2_DGT_ISO = 'US'
    
LEFT JOIN APPOTHER.CUST_INS_WEIGHTS w
  ON sr.CASE_ID = w.CASE_ID

  WHERE sr.QSTN_ID NOT IN ('Q1','Q2_8')
  AND sr.RSPNS_ID <> '9'
  AND ca.FSCL_YR_NUM IN (2018) 
  AND ca.FSCL_QTR_IN_YR_NUM IN (1,2,3)

GROUP BY
  TO_CHAR(sr.TRANS_DTM, 'HH24') 
  ,ca.DAY_IN_CAL_WK_NUM
  ,sr.QSTN_ID
  ,ca.FSCL_YR_NUM
  ,sr.STORE_NUM
)
SELECT 
sq.QSTN_ID
,sq.DAY_PART
,sq.WEEK_PART
,SUM(sq.TOTAL_TB) AS TOTAL_TB
,SUM(sq.TOTAL_RSPNS) AS TOTAL_RSPNS
,ROUND(SUM(sq.TOTAL_TB)/SUM(sq.TOTAL_RSPNS),3) AS TB_SCORE
,sq.FSCL_YR_NUM
,sq.STORE_NUM
FROM sq

WHERE sq.DAY_PART IN ('1_am','2_pm') AND sq.WEEK_PART IN ('1_mf','2_wknd')

GROUP BY 
sq.QSTN_ID
,sq.DAY_PART
,sq.WEEK_PART
,sq.FSCL_YR_NUM
,sq.STORE_NUM

ORDER BY sq.FSCL_YR_NUM
,sq.STORE_NUM
,sq.WEEK_PART
,sq.DAY_PART







WITH sq AS
(SELECT DISTINCT
  TO_CHAR(sr.TRANS_DTM, 'HH24') AS HOUR
    ,(CASE WHEN TO_CHAR(sr.TRANS_DTM, 'HH24') < 11 THEN '1_pre11' -- midday
        WHEN TO_CHAR(sr.TRANS_DTM, 'HH24') >=11 THEN '2_post11' -- pm
        ELSE 'NA' 
        END) AS DAY_PART
  --,(CASE WHEN ca.DAY_IN_CAL_WK_NUM in (2,3,4,5,6) THEN '1_mf' 
  --      WHEN ca.DAY_IN_CAL_WK_NUM in (1,7) THEN '2_wknd' 
  --      ELSE 'NA' 
  --      END) AS WEEK_PART
  ,sr.QSTN_ID
  ,SUM(CASE  WHEN sr.RSPNS_ID = '5' AND sr.QSTN_ID = 'Q1' THEN COALESCE(w.WEIGHT_RT,1) /* coalesce returns non-null weight value, OR if null, returns 1 */
    WHEN sr.RSPNS_ID = '7' THEN COALESCE(w.WEIGHT_RT,1) ELSE 0 END) AS TOTAL_TB
  ,SUM(COALESCE(w.WEIGHT_RT,1)) AS TOTAL_RSPNS
  ,ca.FSCL_YR_NUM
  ,sr.STORE_NUM

FROM APPOTHER.AFT_CV_SRVY_RSPNS sr

JOIN APPCA.D_CAL ca
  ON ca.CAL_DT = TRUNC(sr.TRANS_DTM)
  
JOIN APPCA.D_STORE_VERS st
  ON sr.STORE_NUM = st.STORE_NUM
    AND st.CURRENT_FLG = 'Y'
    AND st.OWNR_TYPE_CD = 'CO'
    AND st.CNTRY_CD_2_DGT_ISO = 'US'
    
LEFT JOIN APPOTHER.CUST_INS_WEIGHTS w
  ON sr.CASE_ID = w.CASE_ID

  WHERE sr.QSTN_ID IN ('Q2_2','Q2_1','Q2_3','Q2_4','Q2_5','Q2_6','Q2_7')
  AND sr.RSPNS_ID <> '9'
  AND ca.FSCL_YR_NUM IN (2018) 
  AND ca.FSCL_QTR_IN_YR_NUM IN (2,3)

GROUP BY
  TO_CHAR(sr.TRANS_DTM, 'HH24') 
  --,ca.DAY_IN_CAL_WK_NUM
  ,sr.QSTN_ID
  ,ca.FSCL_YR_NUM
  ,sr.STORE_NUM
)
SELECT 
sq.QSTN_ID
,sq.DAY_PART
--,sq.WEEK_PART
,SUM(sq.TOTAL_TB) AS TOTAL_TB
,SUM(sq.TOTAL_RSPNS) AS TOTAL_RSPNS
,ROUND(SUM(sq.TOTAL_TB)/SUM(sq.TOTAL_RSPNS),3) AS TB_SCORE
,sq.FSCL_YR_NUM
,sq.STORE_NUM
FROM sq

WHERE sq.DAY_PART IN ('1_pre11','2_post11') --AND sq.WEEK_PART IN ('1_mf','2_wknd')

GROUP BY 
sq.QSTN_ID
,sq.DAY_PART
--,sq.WEEK_PART
,sq.FSCL_YR_NUM
,sq.STORE_NUM

ORDER BY sq.FSCL_YR_NUM
,sq.STORE_NUM
--,sq.WEEK_PART
,sq.DAY_PART


