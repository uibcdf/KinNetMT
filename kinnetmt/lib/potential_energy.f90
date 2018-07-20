MODULE GLOB

CONTAINS

    #SUBROUTINE LOCAL_MINIMA (T_ind,T_w,T_start,N_nodes,Ktot,Is_Minimum)
    #
    #    IMPLICIT NONE
    #
    #    TYPE int_pointer
    #        INTEGER,DIMENSION(:),POINTER::ip
    #    END TYPE int_pointer
    #
    #    TYPE double_pointer
    #        DOUBLE PRECISION,DIMENSION(:),POINTER::dp
    #    END TYPE double_pointer
    #
    #    INTEGER,INTENT(IN)::N_nodes,Ktot
    #    INTEGER,DIMENSION(Ktot),INTENT(IN)::T_ind
    #    DOUBLE PRECISION,DIMENSION(N_nodes),INTENT(IN)::T_w
    #    INTEGER,DIMENSION(N_nodes+1),INTENT(IN)::T_start
    #
    #    LOGICAL,DIMENSION(N_nodes),INTENT(OUT)::Is_Minimum
    #
    #    INTEGER::ii,jj,kk
    #    LOGICAL::aux_flag
    #    DOUBLE PRECISION::aux_val
    #
    #    Is_Minimum(:)=.False.
    #
    #    DO ii=1,N_nodes
    #        aux_flag=.True.
    #        aux_val=T_w(ii)
    #        DO jj=T_start(ii)+1,T_start(ii+1)
    #            kk=T_ind(jj)
    #            IF(T_w(kk)>aux_val) THEN
    #                aux_flag=.False.
    #                EXIT
    #            END IF
    #        END DO
    #        Is_Minimum(ii)=aux_flag
    #    END DO
    #
    #END SUBROUTINE LOCAL_MINIMA

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

    INTEGER::ii,jj,kk
    LOGICAL::aux_flag
    DOUBLE PRECISION::aux_val

    Is_Minimum(:)=.False.

    DO ii=1,N_nodes
        aux_flag=.True.
        aux_val=T_pe(ii)
        DO jj=T_start(ii)+1,T_start(ii+1)
            kk=T_ind(jj)
            IF(T_pe(kk)<aux_val) THEN
                aux_flag=.False.
                EXIT
            END IF
        END DO
        Is_Minimum(ii)=aux_flag
    END DO

END SUBROUTINE LOCAL_MINIMA_POTENTIAL_ENERGY


SUBROUTINE LANDSCAPE_PES_BOTTOM_UP (T_ind, T_pe, T_start, Nodes_index_bottom_up, N_nodes, Ktot, react_coor_x)

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
    INTEGER,DIMENSION(N_nodes),INTENT(IN)::Nodes_index_bottom_up

    DOUBLE PRECISION,DIMENSION(N_nodes),INTENT(OUT)::react_coor_x

    INTEGER::ii,jj,kk
    INTEGER::n_neighbs, n_neighbs_down
    INTEGER,DIMENSION(:),ALLOCATABLE::neighbs_down
    LOGICAL::aux_flag
    DOUBLE PRECISION::aux_val

    react_coor_x(:)=0.0d0

    DO ii=1,N_nodes

    next_node = Nodes_index_bottom_up(ii)
    next_pe   = T_pe(next_node)

    n_neighbs = T_start(next_node+1)-T_start(next_node)
    n_neighbs_down = 0
    ALLOCATE(neighbs_down(n_neighbs))

    DO jj=T_start(next_node)+1,T_start(next_node+1)
        eval_pe = T_pe(T_ind(jj))
        IF (eval_pe < next_pe) THEN
            n_neighbs_down = n_neighbs_down + i
            neighbs_down(n_neighbs_down) = T_ind(jj)
        END IF
    END DO

    IF (n_neighbs_down == 0) THEN

        react_coor_x(next_node) = 0.0
        num_groups = num_groups+1
        belong_to_group(next_node) = num_groups
        ALLOCATE(group(num_groups)%ip(1))

        group(num_groups)%ip(1) = next_node
        lim_inf(num_groups) = 0
        lim_sup(num_groups) = 0
        cambiador(num_groups) = -1

    ELSE

        n_neighbs_groups = 0
        ALLOCATE(list_aux(n_neighbs_down),filt_aux(num_groups))
        filt_aux(:)=.FALSE.

        DO jj=1,n_neighbs_down
             
            filt_aux (neighbs_down(jj))= .TRUE. 

        END DO

        n_linked_groups = 0

        DO jj=1,num_groups
            IF filt_auxj(jj) == .TRUE. THEN
                n_linked_groups = n_linked_groups + 1
                list_aux(n_linked_groups) = jj
            END DO
        END DO

        IF (n_linked_groups == 1) THEN

            aux_index_group = list_aux(1)

        ELSE

            aux_index_group = MIN(list_aux)

        END IF

        belong_to_group(next_node) = aux_index_group
        switch = cambiador(aux_index_group)*(-1)
        cambiador(aux_index_group) = switch

        IF ( switch == -1) THEN

            xx = lim_inf(aux_index_group) - 1
            lim_inf(aux_index_group) = xx

        ELSE

            xx = lim_sup(aux_index_group) +1
            lim_sup(aux_index_group) = xx

        END IF

        react_coor_x(next_node) = xx

        IF (n_linked_groups > 1) THEN

            DO jj=1,n_linked_groups
                
                group_to_lump = list_aux(jj)
                IF (group_to_lump /= aux_index_group) THEN
                     
                    IF ( switch == -1 ) THEN

                        shift = lim_sup(group_to_lump) - xx
                        DO kk = 1,SIZE(group(group_to_lump)%ip)
                            ll = group_to_lump%ip(kk)



    END IF


    END DO



END MODULE GLOB
