subroutine LIDF_fun(TypeLIDF, na, LIDFa, LIDFb, &
        lidf)
    INTEGER, INTENT(in) :: TypeLidf, na
    REAL*8, INTENT(in) :: LIDFa, LIDFb
    REAL*8, INTENT(out) :: lidf(na)

    IF(TypeLidf == 1) THEN
        CALL dladgen(LIDFa,LIDFb,lidf)
    ELSEIF(TypeLidf == 2) THEN
        CALL calc_LIDF_ellipsoidal(na,LIDFa,lidf)
    ENDIF

    return
end subroutine LIDF_fun
