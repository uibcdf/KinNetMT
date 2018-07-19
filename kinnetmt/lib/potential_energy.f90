MODULE GLOB

CONTAINS

SUBROUTINE LOCAL_MINIMA (T_ind,T_w,T_start,N_nodes,Ktot,Is_Minimum)

    IMPLICIT NONE

    TYPE int_pointer
        INTEGER,DIMENSION(:),POINTER::ip
    END TYPE int_pointer

    TYPE double_pointer
        DOUBLE PRECISION,DIMENSION(:),POINTER::dp
    END TYPE double_pointer

    INTEGER,INTENT(IN)::N_nodes,Ktot
    INTEGER,DIMENSION(Ktot),INTENT(IN)::T_ind
    DOUBLE PRECISION,DIMENSION(N_nodes),INTENT(IN)::T_w
    INTEGER,DIMENSION(N_nodes+1),INTENT(IN)::T_start

    LOGICAL,DIMENSION(N_nodes),INTENT(OUT)::Is_Minimum

    INTEGER::ii,jj
    LOGICAL::aux_flag
    DOUBLE PRECISION::aux_val

    Is_Minimum(:)=.False.

    DO ii=1,N_nodes
        aux_flag=.True.
        aux_val=T_w(ii)
        DO jj=T_start(ii)+1,T_start(ii+1)
            IF(T_w(jj)>aux_val) THEN
                aux_flag=.False.
                EXIT
            END IF
        END DO
        Is_Minimum(ii)=aux_flag
    END DO

END SUBROUTINE LOCAL_MINIMA

SUBROUTINE LOCAL_MINIMA_POTENTIAL_ENERGY (T_ind,T_pe,T_start,N_nodes,Ktot,Is_Minimum)

    IMPLICIT NONE

    TYPE int_pointer
        INTEGER,DIMENSION(:),POINTER::ip
    END TYPE int_pointer

    TYPE double_pointer
        DOUBLE PRECISION,DIMENSION(:),POINTER::dp
    END TYPE double_pointer

    INTEGER,INTENT(IN)::N_nodes,Ktot
    INTEGER,DIMENSION(Ktot),INTENT(IN)::T_ind
    DOUBLE PRECISION,DIMENSION(N_nodes),INTENT(IN)::T_pe
    INTEGER,DIMENSION(N_nodes+1),INTENT(IN)::T_start

    LOGICAL,DIMENSION(N_nodes),INTENT(OUT)::Is_Minimum

    INTEGER::ii,jj
    LOGICAL::aux_flag
    DOUBLE PRECISION::aux_val

    Is_Minimum(:)=.False.

    DO ii=1,N_nodes
        aux_flag=.True.
        aux_val=T_pe(ii)
        DO jj=T_start(ii)+1,T_start(ii+1)
            IF(T_pe(jj)<aux_val) THEN
                aux_flag=.False.
                EXIT
            END IF
        END DO
        Is_Minimum(ii)=aux_flag
    END DO

END SUBROUTINE LOCAL_MINIMA_POTENTIAL_ENERGY


END MODULE GLOB
