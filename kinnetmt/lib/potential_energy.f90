MODULE GLOB

CONTAINS

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


SUBROUTINE BASINS (T_ind, T_pe, T_start, Nodes_index_bottom_up, N_nodes, Ktot, belongs_to_basin)

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

    INTEGER,DIMENSION(N_nodes),INTENT(OUT)::belongs_to_basin

    LOGICAL,DIMENSION(N_nodes):: is_local_minimum, already_visited

    INTEGER::ii,jj
    INTEGER::num_basins

    is_local_minimum(:) = .FALSE.
    already_visited(:) = .FALSE.
    belongs_to_basin(:) = -1

    DO ii=1,N_nodes

        next_node = Nodes_index_bottom_up(ii)
        next_pe   = T_pe(next_node)

        n_neighbs = T_start(next_node+1)-T_start(next_node)
        n_neighbs_down = 0
        ALLOCATE(neighbs_down(n_neighbs))

        DO jj=T_start(next_node)+1,T_start(next_node+1)
            eval_pe = T_pe(T_ind(jj))
            IF (eval_pe < next_pe) THEN
                n_neighbs_down = n_neighbs_down + 1
                neighbs_down(n_neighbs_down) = T_ind(jj)
            END IF
        END DO





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

    INTEGER,DIMENSION(N_nodes),INTENT(OUT)::react_coor_x

    INTEGER::ii,jj,kk,ll,mm,nn,oo
    INTEGER::n_neighbs, n_neighbs_down, switch, xx, group_to_lump, shift
    INTEGER::num_groups, n_neighbs_groups, next_node, n_linked_groups, aux_index_group
    DOUBLE PRECISION::next_pe,eval_Pe
    INTEGER,DIMENSION(:),ALLOCATABLE::neighbs_down
    INTEGER,DIMENSION(:),ALLOCATABLE::list_aux, trasvase_aux
    INTEGER,DIMENSION(N_nodes)::belong_to_group
    LOGICAL,DIMENSION(:),ALLOCATABLE::filt_aux,control

    TYPE(int_pointer),DIMENSION(:),POINTER::group
    INTEGER,DIMENSION(:),ALLOCATABLE::lim_inf,lim_sup,cambiador

    ALLOCATE(control(5000))
    control(:)=.False.

    ALLOCATE(group(5000))
    ALLOCATE(lim_inf(5000),lim_sup(5000),cambiador(5000))

    react_coor_x(:)=0.0d0
    belong_to_group(:)=0
    num_groups=0

    DO ii=1,N_nodes

        next_node = Nodes_index_bottom_up(ii)
        next_pe   = T_pe(next_node)

        n_neighbs = T_start(next_node+1)-T_start(next_node)
        n_neighbs_down = 0
        ALLOCATE(neighbs_down(n_neighbs))

        DO jj=T_start(next_node)+1,T_start(next_node+1)
            eval_pe = T_pe(T_ind(jj))
            IF (eval_pe < next_pe) THEN
                n_neighbs_down = n_neighbs_down + 1
                neighbs_down(n_neighbs_down) = T_ind(jj)
            END IF
        END DO

        IF (n_neighbs_down == 0) THEN

            react_coor_x(next_node) = 0.0
            num_groups = num_groups+1
            belong_to_group(next_node) = num_groups
            ALLOCATE(group(num_groups)%ip(1))
            control(num_groups)=.TRUE.

            group(num_groups)%ip(1) = next_node
            lim_inf(num_groups) = 0
            lim_sup(num_groups) = 0
            cambiador(num_groups) = -1

        ELSE

            n_neighbs_groups = 0
            ALLOCATE(list_aux(n_neighbs_down))
            ALLOCATE(filt_aux(num_groups))
            filt_aux(:)=.FALSE.

            DO jj=1,n_neighbs_down
                filt_aux (belong_to_group(neighbs_down(jj)))= .TRUE. 
            END DO

            n_linked_groups = 0

            DO jj=1,num_groups
                IF (filt_aux(jj) .eqv. .TRUE.) THEN
                    n_linked_groups = n_linked_groups + 1
                    list_aux(n_linked_groups) = jj
                END IF
            END DO

            IF (n_linked_groups == 1) THEN

                aux_index_group = list_aux(1)

            ELSE

                aux_index_group = MINVAL(list_aux(1:n_linked_groups),1)

            END IF

            belong_to_group(next_node) = aux_index_group
 
            mm = SIZE(group(aux_index_group)%ip)
            ALLOCATE(trasvase_aux(mm))
            trasvase_aux(:)=group(aux_index_group)%ip(:)
            DEALLOCATE(group(aux_index_group)%ip)
            ALLOCATE(group(aux_index_group)%ip(mm+1))
            group(aux_index_group)%ip(1:mm)=trasvase_aux(:)
            group(aux_index_group)%ip(mm+1)=next_node
            DEALLOCATE(trasvase_aux) 

            switch = cambiador(aux_index_group)*(-1)
            cambiador(aux_index_group) = switch

            IF ( switch == -1) THEN

                xx = lim_inf(aux_index_group)
                react_coor_x(next_node) = xx - 1
                lim_inf(aux_index_group) = react_coor_x(next_node)
                xx = lim_inf(aux_index_group)

            ELSE

                xx = lim_sup(aux_index_group)
                react_coor_x(next_node) = xx + 1
                lim_sup(aux_index_group) = react_coor_x(next_node)
                xx = lim_sup(aux_index_group) 

            END IF

            IF (n_linked_groups > 1) THEN

                DO jj=1,n_linked_groups
                
                    group_to_lump = list_aux(jj)
                    IF (group_to_lump /= aux_index_group) THEN

                        control(group_to_lump)=.FALSE.

                        mm = SIZE(group(aux_index_group)%ip)
                        nn = SIZE(group(group_to_lump)%ip)

                        ALLOCATE(trasvase_aux(mm))
                        trasvase_aux(:)=group(aux_index_group)%ip(:)
                        DEALLOCATE(group(aux_index_group)%ip)
                        ALLOCATE(group(aux_index_group)%ip(mm+nn))
                        group(aux_index_group)%ip(1:mm)=trasvase_aux(:)
                        group(aux_index_group)%ip((mm+1):(mm+nn))=group(group_to_lump)%ip(:)

                        DO kk = 1,nn
                            ll = group(group_to_lump)%ip(kk)
                            belong_to_group(ll)=aux_index_group
                        END DO

                        IF ( switch == -1 ) THEN

                            oo = lim_sup(group_to_lump)
                            DO kk = 1,nn
                                ll = group(group_to_lump)%ip(kk)
                                react_coor_x(ll) = react_coor_x(ll) - ( oo - (xx - 1))
                            END DO

                            lim_inf(aux_index_group) = lim_inf(aux_index_group) - nn 
                            xx = lim_inf(aux_index_group)

                        ELSE

                            oo = lim_inf(group_to_lump)

                            DO kk = 1,nn
                                ll = group(group_to_lump)%ip(kk)
                                react_coor_x(ll) = react_coor_x(ll) + ( (xx +1) - oo )
                            END DO

                            lim_sup(aux_index_group) = lim_sup(aux_index_group) + nn
                            xx = lim_sup(aux_index_group)

                        END IF

                        DEALLOCATE(trasvase_aux,group(group_to_lump)%ip)

                    END IF

                END DO
            END IF

            DEALLOCATE(list_aux,filt_aux)

        END IF

        DEALLOCATE(neighbs_down)

    END DO

    END SUBROUTINE LANDSCAPE_PES_BOTTOM_UP

END MODULE GLOB
