module Control.Monad.Freer.Yo where

import Prelude
import Control.Monad.Free (Free)

type AsFree
  = forall func end. ((end -> end) -> func end) -> Free func end

class Yonedable d (a :: Type) | d -> a, a -> d where
  yo :: d -> a

instance yo0 :: Yonedable ((term -> term) -> (f term)) (f term) where
  yo ctr = ctr identity
else instance yo1 :: Yonedable (a_1 -> (term -> term) -> (f term)) (a_1 -> f term) where
  yo ctr a_1 = ctr a_1 identity
else instance yo2 :: Yonedable (a_2 -> b_2 -> (term -> term) -> (f term)) (a_2 -> b_2 -> f term) where
  yo ctr a_2 b_2 = ctr a_2 b_2 identity
else instance yo3 :: Yonedable (a_3 -> b_3 -> c_3 -> (term -> term) -> (f term)) (a_3 -> b_3 -> c_3 -> f term) where
  yo ctr a_3 b_3 c_3 = ctr a_3 b_3 c_3 identity
else instance yo4 :: Yonedable (a_4 -> b_4 -> c_4 -> d_4 -> (term -> term) -> (f term)) (a_4 -> b_4 -> c_4 -> d_4 -> f term) where
  yo ctr a_4 b_4 c_4 d_4 = ctr a_4 b_4 c_4 d_4 identity
else instance yo5 :: Yonedable (a_5 -> b_5 -> c_5 -> d_5 -> e_5 -> (term -> term) -> (f term)) (a_5 -> b_5 -> c_5 -> d_5 -> e_5 -> f term) where
  yo ctr a_5 b_5 c_5 d_5 e_5 = ctr a_5 b_5 c_5 d_5 e_5 identity
else instance yo6 :: Yonedable (a_6 -> b_6 -> c_6 -> d_6 -> e_6 -> f_6 -> (term -> term) -> (f term)) (a_6 -> b_6 -> c_6 -> d_6 -> e_6 -> f_6 -> f term) where
  yo ctr a_6 b_6 c_6 d_6 e_6 f_6 = ctr a_6 b_6 c_6 d_6 e_6 f_6 identity
else instance yo7 :: Yonedable (a_7 -> b_7 -> c_7 -> d_7 -> e_7 -> f_7 -> g_7 -> (term -> term) -> (f term)) (a_7 -> b_7 -> c_7 -> d_7 -> e_7 -> f_7 -> g_7 -> f term) where
  yo ctr a_7 b_7 c_7 d_7 e_7 f_7 g_7 = ctr a_7 b_7 c_7 d_7 e_7 f_7 g_7 identity
else instance yo8 :: Yonedable (a_8 -> b_8 -> c_8 -> d_8 -> e_8 -> f_8 -> g_8 -> h_8 -> (term -> term) -> (f term)) (a_8 -> b_8 -> c_8 -> d_8 -> e_8 -> f_8 -> g_8 -> h_8 -> f term) where
  yo ctr a_8 b_8 c_8 d_8 e_8 f_8 g_8 h_8 = ctr a_8 b_8 c_8 d_8 e_8 f_8 g_8 h_8 identity
else instance yo9 :: Yonedable (a_9 -> b_9 -> c_9 -> d_9 -> e_9 -> f_9 -> g_9 -> h_9 -> i_9 -> (term -> term) -> (f term)) (a_9 -> b_9 -> c_9 -> d_9 -> e_9 -> f_9 -> g_9 -> h_9 -> i_9 -> f term) where
  yo ctr a_9 b_9 c_9 d_9 e_9 f_9 g_9 h_9 i_9 = ctr a_9 b_9 c_9 d_9 e_9 f_9 g_9 h_9 i_9 identity
else instance yo10 :: Yonedable (a_10 -> b_10 -> c_10 -> d_10 -> e_10 -> f_10 -> g_10 -> h_10 -> i_10 -> j_10 -> (term -> term) -> (f term)) (a_10 -> b_10 -> c_10 -> d_10 -> e_10 -> f_10 -> g_10 -> h_10 -> i_10 -> j_10 -> f term) where
  yo ctr a_10 b_10 c_10 d_10 e_10 f_10 g_10 h_10 i_10 j_10 = ctr a_10 b_10 c_10 d_10 e_10 f_10 g_10 h_10 i_10 j_10 identity
else instance yo11 :: Yonedable (a_11 -> b_11 -> c_11 -> d_11 -> e_11 -> f_11 -> g_11 -> h_11 -> i_11 -> j_11 -> k_11 -> (term -> term) -> (f term)) (a_11 -> b_11 -> c_11 -> d_11 -> e_11 -> f_11 -> g_11 -> h_11 -> i_11 -> j_11 -> k_11 -> f term) where
  yo ctr a_11 b_11 c_11 d_11 e_11 f_11 g_11 h_11 i_11 j_11 k_11 = ctr a_11 b_11 c_11 d_11 e_11 f_11 g_11 h_11 i_11 j_11 k_11 identity
else instance yo12 :: Yonedable (a_12 -> b_12 -> c_12 -> d_12 -> e_12 -> f_12 -> g_12 -> h_12 -> i_12 -> j_12 -> k_12 -> l_12 -> (term -> term) -> (f term)) (a_12 -> b_12 -> c_12 -> d_12 -> e_12 -> f_12 -> g_12 -> h_12 -> i_12 -> j_12 -> k_12 -> l_12 -> f term) where
  yo ctr a_12 b_12 c_12 d_12 e_12 f_12 g_12 h_12 i_12 j_12 k_12 l_12 = ctr a_12 b_12 c_12 d_12 e_12 f_12 g_12 h_12 i_12 j_12 k_12 l_12 identity
else instance yo13 :: Yonedable (a_13 -> b_13 -> c_13 -> d_13 -> e_13 -> f_13 -> g_13 -> h_13 -> i_13 -> j_13 -> k_13 -> l_13 -> m_13 -> (term -> term) -> (f term)) (a_13 -> b_13 -> c_13 -> d_13 -> e_13 -> f_13 -> g_13 -> h_13 -> i_13 -> j_13 -> k_13 -> l_13 -> m_13 -> f term) where
  yo ctr a_13 b_13 c_13 d_13 e_13 f_13 g_13 h_13 i_13 j_13 k_13 l_13 m_13 = ctr a_13 b_13 c_13 d_13 e_13 f_13 g_13 h_13 i_13 j_13 k_13 l_13 m_13 identity
else instance yo14 :: Yonedable (a_14 -> b_14 -> c_14 -> d_14 -> e_14 -> f_14 -> g_14 -> h_14 -> i_14 -> j_14 -> k_14 -> l_14 -> m_14 -> n_14 -> (term -> term) -> (f term)) (a_14 -> b_14 -> c_14 -> d_14 -> e_14 -> f_14 -> g_14 -> h_14 -> i_14 -> j_14 -> k_14 -> l_14 -> m_14 -> n_14 -> f term) where
  yo ctr a_14 b_14 c_14 d_14 e_14 f_14 g_14 h_14 i_14 j_14 k_14 l_14 m_14 n_14 = ctr a_14 b_14 c_14 d_14 e_14 f_14 g_14 h_14 i_14 j_14 k_14 l_14 m_14 n_14 identity
else instance yo15 :: Yonedable (a_15 -> b_15 -> c_15 -> d_15 -> e_15 -> f_15 -> g_15 -> h_15 -> i_15 -> j_15 -> k_15 -> l_15 -> m_15 -> n_15 -> o_15 -> (term -> term) -> (f term)) (a_15 -> b_15 -> c_15 -> d_15 -> e_15 -> f_15 -> g_15 -> h_15 -> i_15 -> j_15 -> k_15 -> l_15 -> m_15 -> n_15 -> o_15 -> f term) where
  yo ctr a_15 b_15 c_15 d_15 e_15 f_15 g_15 h_15 i_15 j_15 k_15 l_15 m_15 n_15 o_15 = ctr a_15 b_15 c_15 d_15 e_15 f_15 g_15 h_15 i_15 j_15 k_15 l_15 m_15 n_15 o_15 identity
else instance yo16 :: Yonedable (a_16 -> b_16 -> c_16 -> d_16 -> e_16 -> f_16 -> g_16 -> h_16 -> i_16 -> j_16 -> k_16 -> l_16 -> m_16 -> n_16 -> o_16 -> p_16 -> (term -> term) -> (f term)) (a_16 -> b_16 -> c_16 -> d_16 -> e_16 -> f_16 -> g_16 -> h_16 -> i_16 -> j_16 -> k_16 -> l_16 -> m_16 -> n_16 -> o_16 -> p_16 -> f term) where
  yo ctr a_16 b_16 c_16 d_16 e_16 f_16 g_16 h_16 i_16 j_16 k_16 l_16 m_16 n_16 o_16 p_16 = ctr a_16 b_16 c_16 d_16 e_16 f_16 g_16 h_16 i_16 j_16 k_16 l_16 m_16 n_16 o_16 p_16 identity
else instance yo17 :: Yonedable (a_17 -> b_17 -> c_17 -> d_17 -> e_17 -> f_17 -> g_17 -> h_17 -> i_17 -> j_17 -> k_17 -> l_17 -> m_17 -> n_17 -> o_17 -> p_17 -> q_17 -> (term -> term) -> (f term)) (a_17 -> b_17 -> c_17 -> d_17 -> e_17 -> f_17 -> g_17 -> h_17 -> i_17 -> j_17 -> k_17 -> l_17 -> m_17 -> n_17 -> o_17 -> p_17 -> q_17 -> f term) where
  yo ctr a_17 b_17 c_17 d_17 e_17 f_17 g_17 h_17 i_17 j_17 k_17 l_17 m_17 n_17 o_17 p_17 q_17 = ctr a_17 b_17 c_17 d_17 e_17 f_17 g_17 h_17 i_17 j_17 k_17 l_17 m_17 n_17 o_17 p_17 q_17 identity
else instance yo18 :: Yonedable (a_18 -> b_18 -> c_18 -> d_18 -> e_18 -> f_18 -> g_18 -> h_18 -> i_18 -> j_18 -> k_18 -> l_18 -> m_18 -> n_18 -> o_18 -> p_18 -> q_18 -> r_18 -> (term -> term) -> (f term)) (a_18 -> b_18 -> c_18 -> d_18 -> e_18 -> f_18 -> g_18 -> h_18 -> i_18 -> j_18 -> k_18 -> l_18 -> m_18 -> n_18 -> o_18 -> p_18 -> q_18 -> r_18 -> f term) where
  yo ctr a_18 b_18 c_18 d_18 e_18 f_18 g_18 h_18 i_18 j_18 k_18 l_18 m_18 n_18 o_18 p_18 q_18 r_18 = ctr a_18 b_18 c_18 d_18 e_18 f_18 g_18 h_18 i_18 j_18 k_18 l_18 m_18 n_18 o_18 p_18 q_18 r_18 identity
else instance yo19 :: Yonedable (a_19 -> b_19 -> c_19 -> d_19 -> e_19 -> f_19 -> g_19 -> h_19 -> i_19 -> j_19 -> k_19 -> l_19 -> m_19 -> n_19 -> o_19 -> p_19 -> q_19 -> r_19 -> s_19 -> (term -> term) -> (f term)) (a_19 -> b_19 -> c_19 -> d_19 -> e_19 -> f_19 -> g_19 -> h_19 -> i_19 -> j_19 -> k_19 -> l_19 -> m_19 -> n_19 -> o_19 -> p_19 -> q_19 -> r_19 -> s_19 -> f term) where
  yo ctr a_19 b_19 c_19 d_19 e_19 f_19 g_19 h_19 i_19 j_19 k_19 l_19 m_19 n_19 o_19 p_19 q_19 r_19 s_19 = ctr a_19 b_19 c_19 d_19 e_19 f_19 g_19 h_19 i_19 j_19 k_19 l_19 m_19 n_19 o_19 p_19 q_19 r_19 s_19 identity
