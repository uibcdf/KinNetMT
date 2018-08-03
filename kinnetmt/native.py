from numpy import zeros as _np_zeros, exp as _np_exp, asfortranarray as _np_asfortranarray,\
                        argwhere as _argwhere
from .lib.potential_energy import glob as _lib_potential_energy

#import lib as libs
#import .utils/LocalMath as LocalMath
#import .utils/LocalKin as LocalKin
#import numpy as np
#from copy import deepcopy
#from os import system
#from sys import exit


#####################################################################################
##### Networks
#####################################################################################

class cl_node():

    def __init__(self):

        self.label=''
        self.link={}
        self.alt_link={}
        self.k_out=0
        self.k_in=0
        self.k=0
        self.weight=0
        self.cluster=0
        self.component=0
        self.coors=None
        self.color=''
        self.size=''
        self.attribute={}

    def most_weighted_links(self,length=1):
        """ Indexes **of** the ranked N most weighted links.

        :param length: number of links
        :type length: integer
        :return: array of node indexes
        :rtype: list

        .. **note** :: Esto es una nota
        """
        aux_bak=[[self.link[x],x] for x in self.link.keys()]
        aux_bak.sort(reverse=True)
        most_w_destin=[]
        for ii in range(length):
            most_w_destin.append(aux_bak[ii][1])
        return most_w_destin

class cl_cluster():

    def __init__(self):
        """Attributes of a cluster or community
        Args:
            Name blabla

        Returns:
           Bla bla bla
        """
        self.label=''
        self.link={}
        self.alt_link={}
        self.nodes=[]
        self.weightiest_node=0
        self.num_nodes=0
        self.weight=0
        self.k_out=0
        self.k_in=0
        self.k=0

class Network():

    def __init_att__(self):

        self.num_nodes=0
        self.num_clusters=0
        self.num_components=0
        self.node=[]
        self.cluster=[]
        self.component=[]
        self.k_total=0
        self.k_max=0
        self.k_out=0
        self.k_in=0
        self.weight=0
        self.labels={}
        self.clustering_method=' '
        self.directed=False
        self.kinetic=False
        self.potential_energy=False
        self.coors=None

        self.file_net=None
        self.file_labels=None

        self.__symmetric__=False

        pass

    def __init_Ts__(self):

        self.Ts=False
        self.T_ind=[]
        self.T_start=[]
        self.T_wl=[]
        self.T_wn=[]

        pass

    def __init__(self, timeseries=None, item=None, file_net=None, file_labels=None,
                 net_format='text', labels_format='text', directed=False, verbose=False):

        self.__init_att__()
        self.__init_Ts__()
        self.directed=directed
        self.kinetic=False
        self.potential_energy=False

        if item is not None:
            self.load_item(item)

        #if file_net!=None:
        #    self.load_net(file_net,net_format,verbose)
        #    if file_labels!=None:
        #        self.load_labels(file_labels,labels_format)
        #elif timeseries!=None:
        #    self.load_net_from_timeseries(file_net,net_format,verbose)

        if verbose:
            self.info()

        pass


    def info(self,update=True,verbose=True):

        if update:

            self.num_nodes=len(self.node)
            self.weight=0
            self.k_total=0
            self.k_max=0
            if self.kinetic:
                for ii in range(self.num_nodes):
                    self.node[ii].weight=sum(self.node[ii].link.values())
            for ii in range(self.num_nodes):
                self.weight+=self.node[ii].weight
                k=len(self.node[ii].link)
                self.node[ii].k_out=k
                self.node[ii].k=k
                self.k_total+=k
                if (self.k_max<k): self.k_max=k

        if verbose:

            print ('# Network:')
            if self.potential_energy:
                print('# PotentialEnergyNetwork')
            if self.kinetic:
                print('# KineticNetwork')
            print ('#', self.num_nodes, 'nodes')
            print ('#', self.k_total, 'links out')
            print ('#', list(self.node[0].attribute.keys()),'additional node attributes')
            if self.kinetic:
                print('#',self.weight,'total weight')

        pass

    def add_node(self, label=None, weight=0, attribute=None, iout=False):
        """Description add node"""

        node=cl_node()
        node.weight=weight
        self.weight+=weight
        no=self.num_nodes
        self.num_nodes+=1

        if label is not None:
            node.label=str(label)
            self.labels[node.label]=no

        if attribute is not None:
            node.attribute=attribute

        self.node.append(node)
        del(node)

        if iout:
            return no
        pass

    def init_empty_nodes(self,num_nodes=None):
        self.node=[cl_node() for ii in xrange(num_nodes)]
        self.num_nodes=num_nodes

    def add_link(self,node_origin,node_final,weight=0,index_origin=False,index_final=False,iout=False):

        if index_origin:
            no=node_origin
        else:
            try:
                no=self.labels[str(node_origin)]
            except:
                no=self.add_node(node_origin,iout=True)

        if index_final:
            nf=node_final
        else:
            try:
                nf=self.labels[str(node_final)]
            except:
                nf=self.add_node(node_final,iout=True)

        try:
            self.node[no].link[nf]+=weight
        except:
            self.node[no].link[nf]=weight

        if not self.directed:
            try:
                self.node[nf].link[no]+=weight
            except:
                self.node[nf].link[no]=weight

        if iout:
            return no, nf

        pass

    def build_Ts(self,alt_links=False):

        if alt_links:

            alt_k_total=0
            for ii in self.node:
                alt_k_total+=len(ii.alt_link[ii])

            alt_T_ind=_np_zeros(shape=(alt_k_total),dtype=int,order='Fortran')
            alt_T_start=_np_zeros(shape=(self.num_nodes+1),dtype=int,order='Fortran')
            alt_T_wl=_np_zeros(shape=(alt_k_total),dtype=float,order='Fortran')

            kk=0
            for ii in range(self.num_nodes):
                alt_T_start[ii]=kk
                aux_links=self.node[ii].alt_link.keys()
                if ii in aux_links:
                    alt_T_ind[kk]=ii+1
                    alt_T_wl[kk]=self.node[ii].alt_link[ii]
                    aux_links.remove(ii)
                    kk+=1
                for jj in aux_links:
                    alt_T_ind[kk]=jj+1
                    alt_T_wl[kk]=self.node[ii].alt_link[jj]
                    kk+=1
            alt_T_start[self.num_nodes]=kk

            return alt_k_total, alt_T_start, alt_T_ind, alt_T_wl

        else:

            self.info(verbose=False)

            self.T_ind=_np_zeros(shape=(self.k_total),dtype=int,order='Fortran')
            self.T_start=_np_zeros(shape=(self.num_nodes+1),dtype=int,order='Fortran')
            self.T_wl=_np_zeros(shape=(self.k_total),dtype=float,order='Fortran')
            self.T_wn=_np_zeros(shape=(self.num_nodes),dtype=float,order='Fortran')

            kk=0
            for ii in range(self.num_nodes):
                self.T_wn[ii]=self.node[ii].weight
                self.T_start[ii]=kk
                aux_links=self.node[ii].link.keys()
                if ii in aux_links:
                    self.T_ind[kk]=ii+1
                    self.T_wl[kk]=self.node[ii].link[ii]
                    aux_links.remove(ii)
                    kk+=1
                for jj in aux_links:
                    self.T_ind[kk]=jj+1
                    self.T_wl[kk]=self.node[ii].link[jj]
                    kk+=1
            self.T_start[self.num_nodes]=kk
            self.Ts=True

        pass

    def build_from_Ts(self):

        if self.Ts:
            self.__init_att__()
            for ii in range(len(self.T_start)-1):
                self.add_node()
                for jj in range(self.T_start[ii],self.T_start[ii+1]):
                    self.node[ii].link[self.T_ind[jj]-1]=self.T_wl[jj]

            self.kinetic=True
            self.info(update=True,verbose=False)

        else:
            print('# Ts arrays needed and self.Ts==True.')
            pass

    def remove_Ts(self):

        self.__init_Ts__()
        pass

    def merge_net(self,net=None,traj=None,verbose=True):

        # merging the labels and weights of nodes

        net_to_total=_np_zeros(net.num_nodes,dtype=int)
        labels_aux=deepcopy(self.labels)
        for ii in xrange(net.num_nodes):
            try :
                no=labels_aux[net.node[ii].label]
            except:
                no=self.add_node(net.node[ii].label,iout=True)
            self.node[no].weight+=net.node[ii].weight
            net_to_total[ii]=no

        # merging the links

        for no in range(net.num_nodes):
            no2=net_to_total[no]
            for nf,wf in net.node[no].link.iteritems():
                nf2=net_to_total[nf]
                self.add_link(no2,nf2,weight=wf,index_origin=True,index_final=True)

        self.info(update=True,verbose=verbose)

        #No estaba comentado#if traj!=None:
        #No estaba comentado#    LocalKin.trans_traj_nodes(net_to_total,traj,net.num_nodes,traj.shape[0],traj.shape[1],traj.shape[2])

        del(net_to_total); del(labels_aux)

        pass

    def extract_net_clusters(self,verbose=True):

        self.clusters_links(verbose=False)

        temp = Network(verbose=False)

        temp.num_nodes= self.num_clusters
        for ii in xrange(temp.num_nodes):
            node=cl_node()
            cluster=self.cluster[ii]
            node.weight = cluster.weight
            node.label  = cluster.label
            node.link   = cluster.link.copy() 
            temp.labels[cluster.label]= ii
            temp.node.append(node)

        temp.weight=self.weight
        temp.Ts=False
        temp.build_Ts()

        if self.__symmetric__:
            temp.__symmetric=True

        if self.kinetic:
            temp.kinetic=True

        if self.directed:
            temp.directed=True

        temp.info(update=True,verbose=verbose)

        del(node)

        return temp


    def extract_net(self,nodes=None,verbose=True):

        nodes=np.array(nodes,dtype=int)

        if self.Ts==False:
            self.build_Ts()

        new_N_nodes=nodes.shape[0]

        new_k_total=libs.net.extract_net_new_k_total(nodes,self.T_ind,self.T_start,new_N_nodes,self.num_nodes,self.k_total)
        pfff=libs.net.extract_net(new_k_total,nodes,self.T_ind,self.T_wl,self.T_start,self.num_nodes,self.k_total,new_N_nodes)

        labels={}
        for ii in range(len(nodes)):
            labels[self.node[nodes[ii]].label]=ii


        temp = Network(verbose=False,)
        temp.num_nodes = new_N_nodes
        temp.k_total = new_k_total
        temp.k_max=pfff[0]
        temp.T_wl=pfff[1]
        temp.T_ind=pfff[2]
        temp.T_start=pfff[3]
        temp.Ts=True
        temp.weight=0
        temp.labels={}
        temp.node=[]

        for ii in range(temp.num_nodes):
            node=cl_node()
            for jj in range(temp.T_start[ii],temp.T_start[ii+1]):
                neigh=temp.T_ind[jj]
                node.link[neigh-1]=temp.T_wl[jj]
            node.k_out=temp.T_start[ii+1]-temp.T_start[ii]
            node.weight=sum(node.link.values())
            temp.weight+=node.weight
            temp.node.append(node)

        if self.kinetic:
            temp.kinetic=True

        if self.directed:
            temp.directed=True

        temp.info(update=True,verbose=verbose)

        for kk,vv in labels.iteritems():
            temp.node[vv].label=kk

        temp.labels=labels

        del(pfff)
        del(labels)

        return temp


    #def load_net_from_timeseries(self,timeseries=None,verbose=True):
    #
    #    self.kinetic=True
    #    self.directed=True

    def load_item(self,item):

        from .multitool import get_form as _get_form
        type_form=_get_form(item)

        if type_form=='networkx.Graph':
            for node in item:
                self.add_node(label=node,attribute=item.node[node])
            for edges in item.edges:
                self.add_link(edges[0],edges[1])

    def load_net(self,name_file,format='text',verbose=True):
        """format:['text','native']"""

        self.file_net=name_file

        fff=open(name_file,'r')

        if format=='text':
            io=cl_io()
            for line in fff.readlines():
                if line[0]=='@':
                    io.read_H1(line)
                    if io.v['with_index']:
                        for ii in range(io.v['num_nodes']):
                            self.add_node('')
                        self.labels={}

                if line[0]=='#':
                    io.read_H2(line)
                else:
                    to_read=io.read_line(line)
                    if to_read:
                        if io.v['with_links']:
                            self.add_link(io.v['node1'],io.v['node2'],weight=io.v['weight'],index_origin=io.v['with_index'],index_final=io.v['with_index'])
                            if not io.v['directed']:
                                self.add_link(io.v['node2'],io.v['node1'],weight=io.v['weight'],index_origin=io.v['with_index'],index_final=io.v['with_index'])
                        else:
                            if io.v['with_index']:
                                self.node[io.v['node1']].weight=io.v['weight']
                            else:
                                io.v['node1']=self.add_node(io.v['node1'],weight=io.v['weight'],iout=True)
                            if io.v['with_coors']: self.node[io.v['node1']].coors=[io.v['coorx'],io.v['coory'],io.v['coorz']]
                            if io.v['with_cluster']: self.node[io.v['node1']].cluster=io.v['cluster']
                            if io.v['with_color']: self.node[io.v['node1']].color=io.v['color']
                            if io.v['with_size']: self.node[io.v['node1']].size=io.v['size']
                            if io.v['with_atts']: self.node[io.v['node1']].att1=io.v['att1']

            self.directed=False
            if io.v['directed']:
                self.directed=True

            self.kinetic=False
            if io.v['kinetic']:
                self.kinetic=True
                for ii in self.node:
                    ii.weight=sum(ii.link.values())

            if io.v['with_cluster']:
                jj=0
                # todo

            del(io)

        if format=='native':

            line=fff.readline()
            self.num_nodes=int(line.split()[0])
            
            k_max=int(line.split()[1])
            k_total=int(line.split()[2])
            
            self.T_ind=_np_zeros(shape=(k_total),dtype=int,order='Fortran')
            self.T_start=_np_zeros(shape=(self.num_nodes+1),dtype=int,order='Fortran')
            self.T_wl=_np_zeros(shape=(k_total),dtype=float,order='Fortran')
            
            data=fff.read()

            data2=[]
            for aa in data.split():
                data2.append(float(aa))
                

            jj=-1
            sumk=0
            for ii in range(self.num_nodes):
                jj+=1

                node=cl_node()
                node_ind=int(data2[jj])
                k_out=int(data2[jj+1])
                weight=data2[jj+2]
                self.T_start[ii]=sumk
                
                jj=jj+2
                for kk in range(k_out):
                    jj+=1
                    neigh=int(data2[jj])
                    jj+=1
                    flux=data2[jj]
                    self.T_ind[sumk]=neigh
                    self.T_wl[sumk]=flux
                    sumk+=1
                    node.link[neigh-1]=flux

                node.k_out=len(node.link)
                node.weight=sum(node.link.values())
                self.node.append(node)

            self.T_start[self.num_nodes]=sumk

            self.k_max=k_max
            self.num_links=0
            for ii in self.node:
                self.num_links+=ii.k_out
            self.k_total=k_total
            self.Ts=True
        

        self.weight=0
        for ii in self.node:
            self.weight+=ii.weight

        self.info(verbose=verbose)
        fff.close()
        pass

###    def load_labels(self,name_file,format='text'):
###
###        """format=[text,water]"""
###
###        self.file_labels=name_file
###
###        fff=open(name_file,'r')
###
###        if format == 'water':
###            for ii in range(self.num_nodes):
###                line=fff.readline()
###                mss=line.split()[1]+' |'
###                for jj in range(2,6):
###                    mss=mss+' '+line.split()[jj]
###                    mss=mss+' |' 
###                for jj in range(6,9):
###                    mss=mss+' '+line.split()[jj]
###                    mss=mss+' |'
###                for jj in range(9,12):
###                    mss=mss+' '+line.split()[jj]
###                    mss=mss+' |'
###                for jj in range(12,15):
###                    mss=mss+' '+line.split()[jj]
###                    mss=mss+' |'
###                for jj in range(15,18):
###                    mss=mss+' '+line.split()[jj]
###                index=int(line.split()[0])-1
###                self.labels[mss]=index
###                self.node[index].label=mss
###
###        if format == 'text':
###            line=fff.readline()
###            line=line.replace('#','')
###            line=line.split()
###            num_fields=len(line)
###            for ind in range(num_fields):
###                if line[ind]=='index':
###                    nind=ind
###                if line[ind]=='label':
###                    nlab=ind
###            for line in fff.readlines():
###                line=line.split()
###                ind=int(line.pop(nind))
###                lab=str(' ').join(line)
###                self.node[ind].label=lab
###                self.labels[lab]=ind
###
###        if len(self.labels)>self.num_nodes:
###            print('# Some labels have no node')
###        if len(self.labels)<self.num_nodes:
###            print('# Some nodes have no label')
###
###        fff.close()

###    def write_labels (self,name_file=None,format='text'):
###
###        # if name_file is None:
###        #     print('# Error: name_file required')
###        #     pass
###
###        # fff=open(name_file,'w')
###        # print('#','index','label',file=fff)
###        # for ii in range(self.num_nodes):
###        #     print(ii,self.node[ii].label,file=fff)
###
###        # fff.close()
###
###        pass

###    def write_net (self,name_file=None,format='text',pymol=False,with_index=True,with_clusters=False, with_atts=False):
###
###        if name_file is None:
###            print('# Error: name_file required')
###            pass
###
###        #todo: check if the file exists
###
###        if format=='native':
###
###            fff=open(name_file,'w')
###
###            print >> fff, self.num_nodes, self.k_max, self.k_total
###
###            for ii in range(self.num_nodes):
###                aux=[]
###                aux.append(ii+1)
###                aux.append(self.node[ii].k_out)
###                aux.append(self.node[ii].weight)
###                if ii in self.node[ii].link.keys():
###                    aux.append(ii+1)
###                    aux.append(self.node[ii].link[ii])
###                for jj in self.node[ii].link.keys():
###                    if jj!=ii:
###                        aux.append(jj+1)
###                        aux.append(self.node[ii].link[jj])
###                aux=str(aux).replace(',','')
###                aux=aux.replace('[','')
###                aux=aux.replace(']','')
###                print >> fff,aux
###
###            fff.close()
###
###        elif format=='text':
###
###            io=cl_io()
###            io.v['num_nodes']=self.num_nodes
###            io.v['with_index']=with_index
###            io.v['directed']=self.directed
###            io.v['kinetic']=self.kinetic
###
###            fff=open(name_file,'w')
###
###            header=[]
###            if io.v['with_index']:
###                header.append('num_nodes='+str(self.num_nodes))
###                header.append('with_index='+str(with_index))
###            header.append('directed='+str(self.directed))
###            if io.v['kinetic']:
###                header.append('kinetic='+str(self.kinetic))
###            print >> fff, '@ ',', '.join(header)
###
###            io.v['with_weight']=True
###            io.v['with_coors']=False
###            io.v['with_cluster']=with_clusters
###            io.v['with_color']=False
###            io.v['with_size']=False
###            io.v['with_atts']=False
###
###            if pymol:
###                io.v['with_coors']=True
###                io.v['with_cluster']=True
###                io.v['with_atts']=True
###
###            line=[]
###            line.append('node')
###            if io.v['with_weight']: line.append('weight')
###            if io.v['with_cluster']: line.append('cluster')
###            if io.v['with_size']: line.append('size')
###            if io.v['with_color']: line.append('color')
###            if io.v['with_coors']: line.append('coorx'); line.append('coory'); line.append('coorz')
###            if io.v['with_atts']: line.append('att1')
###            print >> fff,'# ',' '.join(line)
###
###            for ii in range(self.num_nodes):
###                line=[]
###                node=self.node[ii]
###                if io.v['with_index']:
###                    line.append(str(ii))
###                else:
###                    line.append(node.label)
###                if io.v['with_weight']: line.append(str(node.weight))
###                if io.v['with_cluster']: line.append(str(node.cluster))
###                if io.v['with_size']: line.append(str(node.size))
###                if io.v['with_color']: line.append(str(node.color))
###                if io.v['with_coors']:
###                    line.append('  ')
###                    aux=[0.0,0.0,0.0]
###                    for jj in range(len(node.coors)):
###                        aux[jj]=node.coors[jj]
###                    for jj in aux: line.append(str(jj))
###                if io.v['with_atts']: line.append(str(node.att1))
###                print >> fff,' '.join(line)
###            
###            print >> fff,' '
###            print >> fff,'# node node weight'
###            
###            for ii in range(self.num_nodes):
###                for (jj,v) in self.node[ii].link.iteritems():
###                    line=[]
###                    if io.v['with_index']:
###                        line.append(str(ii))
###                        line.append(str(jj))
###                    else:
###                        line.append(self.node[ii].label)
###                        line.append(self.node[jj].label)
###                    line.append(str(v))
###                    print >> fff,' '.join(line)
###            
###            del(io)
###            fff.close()
###            


############## FUNCTIONS FOR NETWORKS

    def k_distribution (self, option='out', bins=20, range=None,norm=None):

        """ option=['total','in','out']"""

        for ii in self.node:
            ii.k_out=len(ii.link)

        scratch=[]
        if option=='out':
            for ii in self.node:
                scratch.append(ii.k_out)

        xx,yy=LocalMath.Histo1D(scratch,range=range,num_bins=bins,norm=norm)

        return xx, yy

    def weight_distribution (self, objects='nodes', option='all', bins=None, range=range, norm=False,cumul=False):

        if objects in ['nodes','Nodes']:
            """option=['all','self_links','all-self_links','links']  (I have to do the same for the links of a node)"""
            scratch=_np_zeros(self.num_nodes,dtype=float)

            if option=='all':
                for ii in xrange(self.num_nodes):
                    scratch[ii]=self.node[ii].weight

            if option=='self_links':
                for ii in range(self.num_nodes):
                    try:
                        scratch[ii]=self.node[ii].link[ii]
                    except:
                        scratch[ii]=0.0

            if option=='all-self_links':
                for ii in range(self.num_nodes):
                    try:
                        scratch[ii]=(self.node[ii].weight-self.node[ii].link[ii])
                    except:
                        scratch[ii]=self.node[ii].weight

            if cumul==False:
                print('option not implemented')

        xx,yy=LocalMath.Histo1D(scratch,range=range,num_bins=bins,norm=norm)
        
        return xx, yy

    def fpt (self, node_origin, node_sink, num_runs=200, option='mean', bins=20, range=range,norm=None):

        """option=['mean','distribution','both','raw']"""

        if self.Ts==False :

            self.build_Ts()

        scratch=[]
        for ii in range(num_runs):
            iseed=np.random.random_integers(0,4094,4)
            iseed[3]=(iseed[3]/2)*2+1
            aa=libs.net.brownian_run_fpt(self.T_start,self.T_ind,self.T_wl,iseed,node_origin,node_sink,self.num_nodes,self.k_total)
            scratch.append(aa)

        if option in ['distribution','both']:
            xx,yy=LocalMath.Histo1D(scratch,range=range,num_bins=bins,norm=norm)

        if option in ['mean','both']:
            yy_av   =np.array(scratch).mean()
            yy_sigma=np.array(scratch).std()

        if option=='distribution': return xx,yy
        if option=='mean': return yy_av, yy_sigma
        if option=='both': return yy_av, yy_sigma, xx,yy
        if option=='raw':  return scratch

        pass

    def prueba_fpt (self,length=None):

        if length is None:
            print('# length needed.')
            pass

        if self.Ts==False :
            self.build_Ts()

        iseed=np.random.random_integers(0,4094,4)
        iseed[3]=(iseed[3]/2)*2+1

        scratch=libs.net.prueba_fpt(self.T_start,self.T_ind,self.T_wl,iseed,length,self.num_nodes,self.k_total)

        return scratch

    def criterio_distancia (self,mat_in=None):

        scratch=libs.net.criterio_distancia(mat_in,self.num_nodes)

        return scratch


    def brownian_walker (self,origin=0,length=None,self_links=True):

        if length is None:
            print('# length needed.')
            pass

        if self.Ts==False :
            self.build_Ts()

        iseed=np.random.random_integers(0,4094,4)
        iseed[3]=(iseed[3]/2)*2+1

        if self_links:
            scratch=libs.net.brownian_run(1,self.T_start,self.T_ind,self.T_wl,iseed,origin,length,self.num_nodes,self.k_total)
        else:
            scratch=libs.net.brownian_run(0,self.T_start,self.T_ind,self.T_wl,iseed,origin,length,self.num_nodes,self.k_total)

        return scratch

    def matrix_fpt (self):

        mat_fpt=libs.net.matrix_fpt(self.T_start,self.T_ind,self.T_wl,self.num_nodes,self.k_total)

        return mat_fpt

    def min_distance (self):

        pass


    def transition_matrix(self,sparse=False):

        tmatrix=_np_zeros((self.num_nodes,self.num_nodes))

        for ii in xrange(self.num_nodes):
            for jj,kk in self.node[ii].link.iteritems():
                tmatrix[ii,jj]=kk
            norm=tmatrix[ii,:].sum()
            tmatrix[ii,:]=tmatrix[ii,:]

        return tmatrix

    def relaxation_modes (self,num_eigenvals='ALL', only_eigenvals=False):

        if num_eigenvals in ['All','all','ALL']:
            num_eigenvals=self.num_nodes

        if self.Ts==False :

            self.build_Ts

        print('aqui')
        eigenvals_r, eigenvals_i, eigenvects = libs.net.relaxation_modes(num_eigenvals,self.T_start,self.T_ind,self.T_wl
                                                                         ,self.num_nodes,self.k_total)

        return eigenvals_r,eigenvals_i,eigenvects

    def detailed_balance_distance(self,p=1.000):

        if self.Ts==False :

            self.build_Ts()
            
        db_dist=libs.net.detailed_balance_distance(p,self.T_start,self.T_ind,self.T_wl,self.num_nodes,self.k_total)

        return db_dist

    def evolution_step(self,vect_in):

        if self.Ts==False:
            self.build_Ts()

        vect_out=libs.net.evolution_step(self.T_start,self.T_ind,self.T_wl,vect_in,self.num_nodes,self.k_total)

        return vect_out

    def weight_core(self,threshold=None,new=False,verbose=False):

        if threshold is None:
            print('# threshold needed.')
            return

        if self.Ts==False:
            self.build_Ts()

        new_k_total,new_N_nodes=libs.net.weight_core_new_k_total(threshold,self.T_ind,self.T_wl,self.T_start,self.num_nodes,self.k_total)
        pfff=libs.net.weight_core(new_k_total,new_N_nodes,threshold,self.T_ind,self.T_wl,self.T_start,self.num_nodes,self.k_total)
        labels={}
        for ii in range(len(pfff[4])):
            labels[self.node[pfff[4][ii]-1].label]=ii

        if new:
            temp             = Network(verbose=False)
        else:
            temp             = self

        temp.num_nodes = new_N_nodes
        temp.k_total = new_k_total
        temp.k_max=pfff[0]
        temp.T_wl=pfff[1]
        temp.T_ind=pfff[2]
        temp.T_start=pfff[3]
        temp.Ts=True
        temp.weight=0
        temp.labels={}
        temp.node=[]

        for ii in range(temp.num_nodes):
            node=cl_node()
            for jj in range(temp.T_start[ii],temp.T_start[ii+1]):
                neigh=temp.T_ind[jj]
                node.link[neigh-1]=temp.T_wl[jj]
            node.k_out=temp.T_start[ii+1]-temp.T_start[ii]
            node.weight=sum(node.link.values())
            temp.weight+=node.weight
            temp.node.append(node)

        for kk,vv in labels.iteritems():
            temp.node[vv].label=kk

        if self.kinetic:
            temp.kinetic=True

        if self.directed:
            temp.directed=True

        temp.labels=labels

        del(pfff)
        del(labels)

        if verbose:
            temp.info()

        if new:
            return temp
        else:
            pass

    def symmetrize(self,new=False,verbose=False):

        if self.Ts==False :
            self.build_Ts()


        aux_k_total=deepcopy(self.k_total)

        if new:
            temp             = Network(verbose=False)
            temp.labels      = deepcopy(self.labels)
            temp.file_net    = deepcopy(self.file_net)
            temp.file_labels = deepcopy(self.file_labels)
            temp.num_nodes   = deepcopy(self.num_nodes)
            temp.directed    = deepcopy(self.directed)
            temp.kinetic     = deepcopy(self.kinetic)
        else:
            temp             = self


        aux={}
        for ii in range(self.num_nodes):
            for jj in self.node[ii].link.keys():
                aux[(ii,jj)]=0
                aux[(jj,ii)]=0
        temp.k_total=len(aux)
        del(aux)        


        pfff=libs.net.symmetrize_net(temp.k_total,self.T_ind,self.T_wl,self.T_start,self.num_nodes,aux_k_total)

        temp.k_max=pfff[0]
        temp.T_wl=pfff[1]
        temp.T_ind=pfff[2]
        temp.T_start=pfff[3]
        temp.Ts=True
        temp.weight=0
        temp.node=[]
        for ii in range(temp.num_nodes):
            node=cl_node()
            node.weight=pfff[4][ii]
            for jj in range(temp.T_start[ii],temp.T_start[ii+1]):
                neigh=temp.T_ind[jj]
                node.link[neigh-1]=temp.T_wl[jj]
            node.k_out=temp.T_start[ii+1]-temp.T_start[ii]
            temp.weight+=node.weight
            temp.node.append(node)

        for kk,vv in temp.labels.iteritems():
            temp.node[vv].label=kk

        for ii in xrange(temp.num_nodes):
            temp.node[ii].coors=self.node[ii].coors

        temp.__symmetric__=True

        if verbose==True :
            temp.info()

        del(pfff)

        if new:
            return temp
        else:
            pass

    def gradient_clusters_2(self,dim=1,verbose=True):

        if self.Ts==False :

            self.build_Ts()

        self.num_clusters,pfff=libs.net.grad_2(dim,self.T_ind,self.T_wl,self.T_start,self.num_nodes,self.k_total)

        Clust={}
        for ii in range(self.num_nodes):
            try:
                Clust[pfff[ii]].append(ii)
            except:
                Clust[pfff[ii]]=[]
                Clust[pfff[ii]].append(ii)


        aux=np.array(Clust.keys(),dtype=int)
        weight_clusts=_np_zeros((self.num_clusters),dtype=float)
        for ii in range(aux.shape[0]):
            for jj in Clust[aux[ii]]:
                weight_clusts[ii]+=self.node[jj].weight

        tosort=weight_clusts.argsort(kind="mergesort")

        self.cluster=[]

        aa=0
        for ii in range(tosort.shape[0]-1,-1,-1):
            kk=tosort[ii]
            jj=aux[kk]
            temp=cl_cluster()
            temp.label=self.node[jj].label
            temp.nodes=Clust[jj]
            temp.num_nodes=len(temp.nodes)
            temp.weight=weight_clusts[kk]
            for ll in temp.nodes:
                self.node[ll].cluster=aa
            self.cluster.append(temp)
            aa+=1

        del(aux,weight_clusts,tosort,aa)


        # Output: self.clust_info, self.representants, self.node_belongs2, self.cluster_weight, self.num_clusters
        if verbose:
            print('# Number of clusters: ',self.num_clusters)

    def gradient_clusters(self,verbose=True):

        if self.Ts==False :

            self.build_Ts()

        self.num_clusters,pfff=libs.net.grad(self.T_ind,self.T_wl,self.T_start,self.num_nodes,self.k_total)

        Clust={}
        for ii in range(self.num_nodes):
            try:
                Clust[pfff[ii]].append(ii)
            except:
                Clust[pfff[ii]]=[]
                Clust[pfff[ii]].append(ii)


        aux=np.array(Clust.keys(),dtype=int)
        weight_clusts=_np_zeros((self.num_clusters),dtype=float)
        for ii in range(aux.shape[0]):
            for jj in Clust[aux[ii]]:
                weight_clusts[ii]+=self.node[jj].weight

        tosort=weight_clusts.argsort(kind="mergesort")

        self.cluster=[]

        aa=0
        for ii in range(tosort.shape[0]-1,-1,-1):
            kk=tosort[ii]
            jj=aux[kk]
            temp=cl_cluster()
            temp.label=self.node[jj].label
            temp.nodes=Clust[jj]
            temp.num_nodes=len(temp.nodes)
            temp.weight=weight_clusts[kk]
            for ll in temp.nodes:
                self.node[ll].cluster=aa
            self.cluster.append(temp)
            aa+=1

        del(aux,weight_clusts,tosort,aa)


        # Output: self.clust_info, self.representants, self.node_belongs2, self.cluster_weight, self.num_clusters
        if verbose:
            print('# Number of clusters: ',self.num_clusters)

    def clusters_links(self,verbose=True):

        if self.Ts==False:

            self.build_Ts()

        if self.num_clusters < 2:

            print('#Error: Number of clusters lower than 2')
            return

        for ii in xrange(self.num_clusters):
            self.cluster[ii].link={}

        for ii in self.node:
            c_a=ii.cluster
            for jj,ww in ii.link.items():
                c_b=self.node[jj].cluster
                try:
                    self.cluster[c_a].link[c_b]+=ww
                except:
                    self.cluster[c_a].link[c_b]=ww

    def dendo_time(self,steps=1000,verbose=True):

        if self.Ts==False:

            self.build_Ts()

        if self.num_clusters==0:
            return '# Clusters are needed by the algorithm.'

        belongsto=np.empty(shape=(self.num_nodes),dtype=int,order='Fortran')
        for ii in range(self.num_nodes):
            belongsto[ii]=self.node[ii].cluster

        libs.net.dendo_time(steps,self.num_clusters,belongsto,self.T_ind,self.T_wl,self.T_start,self.num_nodes,self.k_total)

        del(belongsto)
        print('# Done')
        return

    def dendo_bottom_up(self,verbose=True):

        if self.Ts==False:
            self.build_Ts()

        if self.num_clusters==0:
            return '# Clusters are needed by the algorithm.'

        belongsto=np.empty(shape=(self.num_nodes),dtype=int,order='Fortran')
        for ii in range(self.num_nodes):
            belongsto[ii]=self.node[ii].cluster

        libs.net.dendo_bottom_up(self.num_clusters,belongsto,self.T_ind,self.T_wl,self.T_start,self.num_nodes,self.k_total)

        del(belongsto)
        print('# Done')
        return

    def dendo_by_nodes(self,verbose=True):

        if self.Ts==False:
            self.build_Ts()

        if self.num_clusters==0:
            return '# Clusters are needed by the algorithm.'

        belongsto=np.empty(shape=(self.num_nodes),dtype=int,order='Fortran')
        for ii in range(self.num_nodes):
            belongsto[ii]=self.node[ii].cluster+1

        libs.net.dendo_by_nodes(self.num_clusters,belongsto,self.T_ind,self.T_wl,self.T_start,self.num_nodes,self.k_total)

        del(belongsto)
        print('# Done')
        return


    def pfold(self,A=0,B=0,num_iter=20000):

        if self.Ts==False:
            self.build_Ts()

        pfold_out=libs.net.pfold(A,B,self.T_ind,self.T_wl,self.T_start,num_iter,self.num_nodes,self.k_total)

        return np.ascontiguousarray(pfold_out)

    def mfpt(self,A=0):

        if self.Ts==False:
            self.build_Ts()

        mfpt_out=libs.net.mfpt(A,self.T_ind,self.T_wl,self.T_start,num_iter,self.num_nodes,self.k_total)

        return np.ascontiguousarray(mfpt_out)

    def cfep(self,reaction_values=None,num_bins=1000,KbT=(0.0019872*300.0),withfortran=True):

        if withfortran:

            if self.Ts==False:
                self.build_Ts()

            opt_bins=1
            if num_bins<1:
                num_bins=self.num_nodes
                opt_bins=0

            cfep_out,key_cfep=libs.net.cfep(opt_bins,self.T_ind,self.T_wl,self.T_start,np.asfortranarray(reaction_values)
                                            ,num_bins,self.num_nodes,self.k_total)

            x_Za_Z=np.ascontiguousarray(cfep_out[0,:])
            y_minoslog_Zab_z=np.ascontiguousarray(cfep_out[1,:])
            aux_react_val=np.ascontiguousarray(cfep_out[2,:])
        else:
            pass


        return x_Za_Z,KbT*y_minoslog_Zab_z,aux_react_val,key_cfep


    def cfep_old(self,mode='pfold',A=0,B=0,num_bins=0,num_iter=20000,KbT=(0.0019872*300.0)):

        if self.Ts==False:

            self.build_Ts()

        if mode=='pfold':

            opt_bins=1
            if num_bins<1:
                num_bins=self.num_nodes
                opt_bins=0
            cfep_out,pfold_vals,key_cfep=libs.net.cfep_pfold3(opt_bins,A,B,self.T_ind,self.T_wl,self.T_start,num_bins,num_iter,KbT,self.num_nodes,self.k_total)
            return cfep_out[:,0],cfep_out[:,1],cfep_out[:,2],key_cfep,pfold_vals


        if mode=='mfpt':

            A=A+1
            cfep_out,key_cfep1,key_cfep2=libs.net.cfep_mfpt(A,self.T_ind,self.T_wl,self.T_start,num_bins,num_iter,self.num_nodes,self.k_total)
            return cfep_out,key_cfep1,key_cfep2

    def dijkstra(self,node='all',alt_links=False):

        if self.Ts==False :
            self.build_Ts()

        if node=='all':
            node=-1
            dim_out=self.num_nodes
        else:
            node=node+1
            dim_out=1

        opt_directed=0
        if self.directed:
            opt_directed=1

        pfff=libs.net.dijkstra(node,dim_out,opt_directed,self.T_start,self.T_ind,self.T_wl,self.num_nodes,self.k_total)

        return pfff

    def mds_con_distancias(self,distancias=None):

        eigenvs=self.num_nodes
        dim=3

        libs.mds.cargo_distancias(distancias,self.num_nodes)
        o_coors,o_eigenvals,o_eigenvects,o_stress=libs.mds.mds(0,dim,eigenvs,self.num_nodes)

        for ii in range(self.num_nodes):
            self.node[ii].coors=o_coors[ii][:]

        return o_eigenvals,o_eigenvects

    def diffusion_distance(self,tt=1):

        libs.mds.load_net(self.T_start,self.T_ind,self.T_wl,self.num_nodes,self.k_total)
        newdists=libs.mds.diffusion_distance(tt,self.num_nodes)

        return newdists

    def distancias_majorization(self,tipo=1,distancias=None):

        oldcoors=_np_zeros((3,self.num_nodes),dtype=float,order='F')
        for ii in xrange(self.num_nodes):
            oldcoors[:,ii]=self.node[ii].coors[:]

        print('1')
        libs.mds.load_net(self.T_start,self.T_ind,self.T_wl,self.num_nodes,self.k_total)
        print('2')
        libs.mds.cargo_distancias(distancias,self.num_nodes)

        print('3')
        newcoors=libs.mds.majorization(tipo,oldcoors,self.num_nodes)

        for ii in xrange(self.num_nodes):
            self.node[ii].coors[:]=newcoors[:,ii]

    def mds2(self,tipo=None,dim=3,eigenvs='all',stress=False,pivots=False,num_pivots=None,extra_pivots=None):

        if pivots==False:

            if self.Ts==False :
                self.build_Ts()

            libs.mds.load_net(self.T_start,self.T_ind,self.T_wl,self.num_nodes,self.k_total)

            if tipo==1:
                libs.mds.pre_relax_1st_order()
            elif tipo==2:
                libs.mds.pre_relax_1st_order2()
            elif tipo==3:
                libs.mds.pre_relax_hamm()
            elif tipo==4:
                libs.mds.pre_inv_flux()
            elif tipo==5:
                libs.mds.pre_min_fpt()
            elif tipo==6:
                libs.mds.pre_ave_fpt()

            libs.mds.dijkstra()
            dxd=deepcopy(libs.mds.dists)
            if eigenvs in ['all','All']:
                eigenvs=self.num_nodes
            if eigenvs>self.num_nodes:
                print('# Error: eigenvs>num_nodes')
                return 
            if dim>eigenvs:
                print('# Error: dim>eigenvs')
                return

            opt_stress=0

            if stress:
                opt_stress=1

            o_coors,o_eigenvals,o_eigenvects,o_stress=libs.mds.mds(opt_stress,dim,eigenvs,self.num_nodes)

        else:

            if pivots in ['random','Random','RANDOM','random2','Random2']:
                if type(num_pivots)==int:

                    if self.Ts==False :
                        self.build_Ts()

                    libs.mds.load_net(self.T_start,self.T_ind,self.T_wl,self.num_nodes,self.k_total)

                    if tipo==1:
                        libs.mds.pre_relax_1st_order()
                    elif tipo==2:
                        libs.mds.pre_relax_1st_order2()
                    elif tipo==3:
                        libs.mds.pre_relax_hamm()
                    elif tipo==4:
                        libs.mds.pre_inv_flux()

                    print('elije')
                    if pivots in ['random','Random','RANDOM']:
                        list_pivots=libs.mds.choose_random_pivots_1(num_pivots)
                        print('dijkstra_pivots')
                        libs.mds.dijkstra_pivots()
                        dxd=deepcopy(libs.mds.dists)
                    if pivots in ['random2','Random2','RANDOM2']:
                        if extra_pivots is None:
                            num_extra_pivs=0
                            extra_pivs=[]
                        else:
                            num_extra_pivs=len(extra_pivots)
                            extra_pivs=np.array(extra_pivots,dtype=int)
                        list_pivots=libs.mds.choose_random_pivots_2_w_dijkstra(num_pivots,extra_pivs,num_extra_pivs)

                    if eigenvs in ['all','All']:
                        eigenvs=self.num_nodes
                    if eigenvs>self.num_nodes:
                        print('# Error: eigenvs>num_nodes')
                        return 
                    if dim>eigenvs:
                        print('# Error: dim>eigenvs')
                        return

                    opt_stress=0

                    if stress:
                        opt_stress=1

                    o_coors=libs.mds.mds_pivots(dim,self.num_nodes)

                else:
                    print('# Error: num_pivots required')
                    return

        for ii in range(self.num_nodes):
            self.node[ii].coors=o_coors[ii][:]

        if pivots==False:
            if stress:
                return o_eigenvals,o_eigenvects,o_stress
            else:
                return o_eigenvals,o_eigenvects, dxd
        else:
            return list_pivots, dxd
            pass



    def mds(self,dim=3,eigenvs='all',output=False,alt_links=False,distances=None,dijkstra=True,stress=False):

        #eigenvs=self.num_nodes
        if eigenvs in ['all','All']:
            eigenvs=self.num_nodes
        if eigenvs>self.num_nodes:
            print('# Error: eigenvs>num_nodes')
            return 
        if dim>eigenvs:
            print('# Error: dim>eigenvs')
            return

        if distances is None: #Just to fill the variable
            distances=_np_zeros(shape=(1,1),dtype=float,order='Fortran')
            dim_distances=1
        else:
            dim_distances=len(distances)

        opt=0
        opt_stress=0
        opt_directed=0
        if self.directed:
            opt_directed=1

        if dijkstra:
            opt=1
        if stress:
            opt_stress=1

        if alt_links:
            alt_k_total, alt_T_start, alt_T_ind, alt_T_wl=self.build_Ts(alt_links=True)
            o_coors,o_eigenvals,o_eigenvects,o_stress=libs.net.mds(opt_directed,opt,opt_stress,dim,eigenvs,alt_T_start,alt_T_ind,alt_T_wl,distances,self.num_nodes,alt_k_total,dim_distances)
            del(alt_k_total, alt_T_start, alt_T_ind, alt_T_wl)
        else:
            if self.Ts==False :
                self.build_Ts()
            o_coors,o_eigenvals,o_eigenvects,o_stress=libs.net.mds(opt_directed,opt,opt_stress,dim,eigenvs,self.T_start,self.T_ind,self.T_wl,distances,self.num_nodes,self.k_total,dim_distances)

        for ii in range(self.num_nodes):
            self.node[ii].coors=o_coors[ii][:]

        if output:
            if stress:
                return o_eigenvals,o_eigenvects,o_stress
            else:
                return o_eigenvals,o_eigenvects
        else:
            pass


    def mcl(self,granularity=1.5,eps=0.005,iterations=0,pruning=True,alt_links=False,verbose=True,ss=True):

        ## I have to reset previous clusters

        if alt_links:

            alt_k_total, alt_T_start, alt_T_ind, alt_T_wl=self.build_Ts(alt_links=True)
            self.num_clusters,pfff=libs.net.mcl(granularity,eps,iterations,alt_T_start,alt_T_ind,alt_T_wl,self.num_nodes,alt_k_total)
            del(alt_k_total, alt_T_start, alt_T_ind, alt_T_wl)

        else:
            if pruning:

                if ss:
                    fff=open('.input_mcl','w')
                    for ii in range(self.num_nodes):
                        for jj,kk in self.node[ii].link.iteritems():
                            if not jj<ii:
                                print(ii,jj,kk,file=fff)
                    fff.close()
                else:
                    fff=open('.input_mcl','w')
                    for ii in range(self.num_nodes):
                        for jj,kk in self.node[ii].link.iteritems():
                            print(ii,jj,kk,file=fff)
                    fff.close()

                # Check whether mcl exists
                if system("type mcl > /dev/null") != 0:
                    print("# Error. Can't find mcl (Markov Cluster Algorithm).")
                    exit()

                comando='mcl .input_mcl --abc -I '+str(granularity)+' -o .output_mcl > /dev/null 2>&1'
                salida=system(comando)

                if salida!=0:
                    print('# Error')
                    exit()
                
                fff=open('.output_mcl','r')
                Clust={}

                cl=0
                for line in fff:    
                    line=line.split()
                    Clust[cl]=[]
                    for aa in line:
                        Clust[cl].append(int(aa))
                    cl+=1

                self.num_clusters=cl
                fff.close()
                salida=system('rm .output_mcl .input_mcl')


            else:
                if self.Ts==False :
                    self.build_Ts()
                    
                self.num_clusters,pfff=libs.net.mcl(granularity,eps,iterations,self.T_start,self.T_ind,self.T_wl,self.num_nodes,self.k_total)

                Clust={}
                 
                for ii in range(self.num_nodes):
                    try:
                        Clust[pfff[ii]].append(ii)
                    except:
                        Clust[pfff[ii]]=[]
                        Clust[pfff[ii]].append(ii)

        aux=np.array(Clust.keys(),dtype=int)
        aux_rep=_np_zeros(aux.shape[0],dtype=int)
        weight_clusts=_np_zeros((self.num_clusters),dtype=float)
        for ii in range(aux.shape[0]):
            wrep=0.0
            jjrep=0
            for jj in Clust[aux[ii]]:
                weight_clusts[ii]+=self.node[jj].weight
                if wrep<self.node[jj].weight:
                    jjrep=jj
                    wrep=self.node[jj].weight
            aux_rep[ii]=jjrep

        tosort=weight_clusts.argsort(kind="mergesort")

        self.cluster=[]
        aa=0
        for ii in range(tosort.shape[0]-1,-1,-1):
            kk=tosort[ii]
            jj=aux[kk]
            jjrep=aux_rep[kk]
            temp=cl_cluster()
            temp.label=self.node[jjrep].label
            temp.nodes=Clust[jj]
            temp.num_nodes=len(temp.nodes)
            temp.weight=weight_clusts[kk]
            for ll in temp.nodes:
                self.node[ll].cluster=aa
            self.cluster.append(temp)
            aa+=1

        del(aux,weight_clusts,tosort,aa)
        #for ii in Clust.keys():
        #    temp=cl_cluster()
        #    temp.label=self.node[int(ii)].label
        #    temp.nodes=Clust[ii]
        #    temp.num_nodes=len(temp.nodes)
        #    temp.weight=0
        #    for jj in temp.nodes:
        #        self.node[jj].cluster=a
        #        temp.weight+=self.node[jj].weight
        #    self.cluster.append(temp)
        #    a+=1


        # Output: self.clust_info, self.representants, self.node_belongs2, self.cluster_weight, self.num_clusters
        if verbose:
            print('# Number of clusters: ',self.num_clusters)

        pass

    def components(self,alt_links=False,verbose=True):

        if alt_links:
            alt_k_total, alt_T_start, alt_T_ind, alt_T_wl=self.build_Ts(alt_links=True)
            self.num_components,pfff=libs.net.components(alt_T_start,alt_T_ind,self.num_nodes,alt_k_total)
            del(alt_k_total, alt_T_start, alt_T_ind, alt_T_wl)
        else:
            if self.Ts==False :
                self.build_Ts()
            self.num_components,pfff=libs.net.components(self.T_start,self.T_ind,self.num_nodes,self.k_total)

        Comp={}

        for ii in range(self.num_nodes):
            try:
                Comp[pfff[ii]].append(ii)
            except:
                Comp[pfff[ii]]=[]
                Comp[pfff[ii]].append(ii)

        aux=np.array(Comp.keys(),dtype=int)
        weight_comps=_np_zeros((self.num_components),dtype=float)
        for ii in range(aux.shape[0]):
            for jj in Comp[aux[ii]]:
                weight_comps[ii]+=self.node[jj].weight

        tosort=weight_comps.argsort(kind="mergesort")

        self.component=[]
        aa=0
        bb=0.0
        bb_ind=0
        gc_num_nodes=0
        gc_nodes=0
        for ii in range(tosort.shape[0]-1,-1,-1):
            kk=tosort[ii]
            jj=aux[kk]
            temp=cl_cluster()
            temp.nodes=Comp[jj]
            temp.num_nodes=len(temp.nodes)
            temp.weight=weight_comps[kk]
            bb=0.0
            for ll in temp.nodes:
                self.node[ll].component=aa
                if (self.node[ll].weight>bb):
                    bb=self.node[ll].weight
                    bb_ind=ll
            temp.label=self.node[bb_ind].label
            self.component.append(temp)
            aa+=1
            if gc_num_nodes<temp.num_nodes:
                gc_num_nodes=temp.num_nodes
                gc_nodes=Comp[jj]

        del(aux,weight_comps,tosort,aa)
        del(Comp)


        if verbose:
            print('# Number of components: ',self.num_components)

        pass

    def giant_component(self,verbose=True):

        if self.Ts==False :
            self.build_Ts()
        self.num_components,pfff=libs.net.components(self.T_start,self.T_ind,self.num_nodes,self.k_total)

        Comp={}

        for ii in range(self.num_nodes):
            try:
                Comp[pfff[ii]].append(ii)
            except:
                Comp[pfff[ii]]=[]
                Comp[pfff[ii]].append(ii)

        aux=np.array(Comp.keys(),dtype=int)
        weight_comps=_np_zeros((self.num_components),dtype=float)
        for ii in range(aux.shape[0]):
            for jj in Comp[aux[ii]]:
                weight_comps[ii]+=self.node[jj].weight

        tosort=weight_comps.argsort(kind="mergesort")

        self.component=[]
        aa=0
        gc_num_nodes=0
        gc_nodes=0
        for ii in range(tosort.shape[0]-1,-1,-1):
            kk=tosort[ii]
            jj=aux[kk]
            temp=cl_cluster()
            temp.nodes=Comp[jj]
            temp.num_nodes=len(temp.nodes)
            temp.weight=weight_comps[kk]
            for ll in temp.nodes:
                self.node[ll].component=aa
            self.component.append(temp)
            aa+=1
            if gc_num_nodes<temp.num_nodes:
                gc_num_nodes=temp.num_nodes
                gc_nodes=Comp[jj]

        del(aux,weight_comps,tosort,aa)
        del(Comp)

        if verbose:
            print('# Nodes in the giant component: ', gc_num_nodes)

        return gc_nodes

        pass


    def buildCoors(self,from_labels='range'):

        if from_labels in ['range','ranges']:
            for node in self.node:
                node.coors=LocalMath.NumpyStr2NumpyArray(node.label)
        pass



class KineticNetwork(Network):
    pass

class PotentialEnergyNetwork(Network):

    def __init__(self, item=None, file_net=None, file_labels=None,
                 net_format='text', labels_format='text', verbose=False):

        super().__init__(self, item=item, file_net=file_net, file_labels=file_labels,
                 net_format=net_format, labels_format='text', verbose=verbose)

        self.potential_energy=True
        self.potential_energies=_np_zeros(self.num_nodes)
        for node_index in range(self.num_nodes):
            self.potential_energies[node_index]=self.node[node_index].attribute['Potential_Energy']._value

        self._energy_units=self.node[0].attribute['Potential_Energy'].unit
        #self.set_thermodynamic_weight()

    def set_thermodynamic_weight(self,temperature=None,Kb=0.0083144621,KbT=2.479):
        #KT a 298K = 2.479 en kJ/mol
        #Kb es 0.0083144621 en kJ/(mol*K)
        self.weight=0.0

        if temperature is not None:
            KbT = Kb * temperature

        for node in self.node:
            node.weight=_np_exp(-node.attribute['Potential_Energy']/KbT)
            self.weight+=node.weight

    def get_local_minima(self):

        if not self.Ts:
            self.build_Ts()

        tmp_filter = _lib_potential_energy.local_minima_potential_energy(self.T_ind,
                                                                      self.potential_energies,
                                                                       self.T_start, self.num_nodes,
                                                                       self.k_total)
        tmp_array = tmp_filter.nonzero()[0]
        del(tmp_filter)
        return tmp_array

    def get_absolute_minimum(self):

        #if not self.Ts:
        #    self.build_Ts()

        #_aux_val=self.T_wn.argmax()

        _aux_val=self.potential_energies.argmin()

        return _aux_val, self.potential_energies[_aux_val]

    def get_potential_energy_1D_landscape(self):

        tmp_xx = self._get_landscape_bottom_up()
        tmp_potential_energies = self.potential_energies
        return tmp_xx, tmp_potential_energies*self._energy_units

    def _get_landscape_bottom_up(self):

        if not self.Ts:
            self.build_Ts()

        nodes_index_bottom_up = _np_asfortranarray(self.potential_energies.argsort()+1)

        react_coor_x =_lib_potential_energy.landscape_pes_bottom_up(self.T_ind,
                                                                    self.potential_energies,
                                                                    self.T_start,
                                                                    nodes_index_bottom_up,
                                                                    self.num_nodes, self.k_total)
        del(nodes_index_bottom_up)
        return react_coor_x

        pass

    def make_potential_energy_basins(self):

        if not self.Ts:
            self.build_Ts()

        nodes_index_bottom_up = _np_asfortranarray(self.potential_energies.argsort()+1)
        tmp_basins = _lib_potential_energy.basins(self.T_ind, self.potential_energies,
                                                  self.T_start, nodes_index_bottom_up, self.num_nodes, self.k_total)

        del(nodes_index_bottom_up)


        self.cluster=[]
        self.num_clusters=tmp_basins.max()+1

        for index_basin in range(self.num_clusters):

            temp_cluster = cl_cluster()
            nodes_in_basin = _argwhere(tmp_basins==index_basin).flatten()
            pes_in_basin = self.potential_energies[nodes_in_basin]
            temp_cluster.nodes = [ii for _,ii in sorted(zip(pes_in_basin,nodes_in_basin))]
            temp_cluster.num_nodes = nodes_in_basin.shape[0]
            ii = temp_cluster.nodes[0]
            temp_cluster.weightiest_node = ii
            temp_cluster.weight = self.node[ii].attribute['Potential_Energy']
            temp_cluster.label = self.node[temp_cluster.weightiest_node].label
            for ll in temp_cluster.nodes:
                self.node[ll].cluster=index_basin
            self.cluster.append(temp_cluster)

        del(nodes_in_basin, pes_in_basin, temp_cluster, tmp_basins)
        pass


#### External Functions

#def traj2net(filename=None,num_particles=0,num_frames=0,output=None):
# 
#    if output is None :
#        ii=filename.rfind('.')
#        if ii>0 :
#            output=filename[:ii]+'.pxn'
#        else:
#            output=filename+'.pxn'
# 
#    if filename.endswith('.bin'):
#        libs.net.build_net_bin(filename,output,num_particles,num_frames)
#    else:
#        libs.net.build_net(filename,output,num_particles,num_frames)
# 
# 
#    print ' # New network file:', output
#    return None

#class traj2net():
# 
#    def __init__(self,traj=None,num_frames=0,num_parts=0,dimension=0,optimized=False):
# 
#        self.optimized=False
#        self.init=False
#        self.num_frames=0
#        self.num_parts=0
#        self.dimension=0
#        self.keys=[]
#        self.coors=[]
#        self.nodes=[]
# 
#        if traj!=None:
#            self.init=True
#            self.coors=traj
#            self.num_frames=len(traj)
#            self.dimension=len(traj[0])
# 
#        if num_frames!=0 and num_parts!=0 and dimension!=0:
#            self.init=True
#            if optimized:
#                self.optimized=True
# 
#    def append_frame(self,frame=None):
# 
#        #if self.init:
#        #    for ii in range(num_parts):
#        #        for jj in range(dim):
#            
#        pass
# 
#    def append_frame(self):
# 
#        pass


def kinetic_network(traj=None,ranges=None,bins=None,traj_out=False,labels=True,verbose=True):
 
    prov_net=Network(directed=True,kinetic=True,verbose=False)
 
    ranges=LocalMath.standard_ranges(ranges)
    dimensions=ranges.shape[0]
    traj=LocalMath.standard_traj(traj=traj,dimensions=dimensions)
    num_frames=traj.shape[0]
    num_parts=traj.shape[1]
 
    opt_labels=0
    if labels:
        opt_labels=1

    if bins!=None:
        if type(bins) in [int]:
            bins=[bins]
        if len(bins)!=dimensions:
            print('# The length of bins must be equal to the length of ranges')
            return
        bins=np.array(bins,dtype=int,order='F')
        traj_net=LocalKin.trajbinning2net(opt_labels,traj,ranges,bins,num_frames,num_parts,dimensions)
    else:
        traj_net=LocalKin.traj2net(opt_labels,traj,ranges,num_frames,num_parts,dimensions)
 
    traj_net=LocalMath.standard_traj(traj_net,particles=num_parts,dimensions=1)
 
    prov_net.Ts=True
    prov_net.T_ind=deepcopy(LocalKin.t_ind)
    prov_net.T_wl=deepcopy(LocalKin.t_tau)
    prov_net.T_start=deepcopy(LocalKin.t_start)
    prov_net.build_from_Ts()
    if opt_labels:
        if bins is None:
            for ii in range(prov_net.num_nodes):
                label=str(LocalKin.labels[ii])
                prov_net.node[ii].label=label
                prov_net.labels[label]=ii
        else:
            for ii in range(prov_net.num_nodes):
                label=str(LocalKin.labels_daux[ii])
                prov_net.node[ii].label=label
                prov_net.labels[label]=ii
 

    LocalKin.free_memory_ts()
    if verbose:
        prov_net.info(update=False,verbose=True)
        pass
    else:
        pass
 
    if traj_out:
        return prov_net,traj_net
    else:
        del(traj_net)
        return prov_net


def kinetic_network_list(traj=None,ranges=None,bins=None,traj_out=False,labels=True,verbose=True):

    prov_net=Network(directed=True,kinetic=True,verbose=False)

    ranges=LocalMath.standard_ranges(ranges)
    dimensions=ranges.shape[0]
    traj=LocalMath.standard_traj(traj=traj,dimensions=dimensions)
    num_frames=traj.shape[0]
    num_parts=traj.shape[1]

    opt_labels=0
    if labels:
        opt_labels=1

    if bins!=None:
        if type(bins) in [int]:
            bins=[bins]
        if len(bins)!=dimensions:
            print('# The length of bins must be equal to the length of ranges')
            return
        bins=np.array(bins,dtype=int,order='F')
        traj_net=LocalKin.trajbinning2net(opt_labels,traj,ranges,bins,num_frames,num_parts,dimensions)
    else:
        traj_net=LocalKin.traj2net(opt_labels,traj,ranges,num_frames,num_parts,dimensions)

    traj_net=LocalMath.standard_traj(traj_net,particles=num_parts,dimensions=1)

    prov_net.Ts=True
    prov_net.T_ind=deepcopy(LocalKin.t_ind)
    prov_net.T_wl=deepcopy(LocalKin.t_tau)
    prov_net.T_start=deepcopy(LocalKin.t_start)
    prov_net.build_from_Ts()
    if opt_labels:
        if bins is None:
            for ii in range(prov_net.num_nodes):
                label=str(LocalKin.labels[ii])
                prov_net.node[ii].label=label
                prov_net.labels[label]=ii
        else:
            for ii in range(prov_net.num_nodes):
                label=str(LocalKin.labels_daux[ii])
                prov_net.node[ii].label=label
                prov_net.labels[label]=ii


    LocalKin.free_memory_ts()
    if verbose:
        prov_net.info(update=False,verbose=True)
        pass
    else:
        pass
 
    if traj_out:
        return prov_net,traj_net
    else:
        del(traj_net)
        return prov_net



## Estaba comentado ##class kinetic_network(network):
## Estaba comentado ## 
## Estaba comentado ##    def __init__(self,traj=None,ranges=None,verbose=True):
## Estaba comentado ## 
## Estaba comentado ##        self.__init_att__()
## Estaba comentado ##        self.__init_Ts__()
## Estaba comentado ## 
## Estaba comentado ##        try:
## Estaba comentado ##            rango_traj=traj.shape
## Estaba comentado ##        except:
## Estaba comentado ##            traj=array(traj,order='Fortran')
## Estaba comentado ##            rango_traj=traj.shape
## Estaba comentado ## 
## Estaba comentado ##        try:
## Estaba comentado ##            rango_ranges=ranges.shape
## Estaba comentado ##        except:
## Estaba comentado ##            ranges=array(ranges,order='Fortran')
## Estaba comentado ##            rango_ranges=ranges.shape
## Estaba comentado ## 
## Estaba comentado ##        ###
## Estaba comentado ## 
## Estaba comentado ##        if len(rango_ranges)==1:
## Estaba comentado ##            ranges=[ranges]
## Estaba comentado ##            ranges=array(ranges,order='Fortran')
## Estaba comentado ##            rango_ranges=ranges.shape
## Estaba comentado ## 
## Estaba comentado ##        dimensions=rango_ranges[0]
## Estaba comentado ##        if (rango_ranges[1]!=2):
## Estaba comentado ##            print '# Error with ranges'
## Estaba comentado ##            pass
## Estaba comentado ## 
## Estaba comentado ##        if len(rango_traj)==1:
## Estaba comentado ##            if dimensions==1:
## Estaba comentado ##                traj=[expand_dims(traj,1)]
## Estaba comentado ##                traj=array(traj,order='Fortran')
## Estaba comentado ##                rango_traj=traj.shape
## Estaba comentado ##            else:
## Estaba comentado ##                print '# Error with dimension of traj and ranges'
## Estaba comentado ##                pass
## Estaba comentado ## 
## Estaba comentado ##        if len(rango_traj)==2:
## Estaba comentado ##            if dimensions>1:
## Estaba comentado ##                traj=[traj]
## Estaba comentado ##                traj=array(traj,order='Fortran')
## Estaba comentado ##                rango_traj=traj.shape
## Estaba comentado ##            else:
## Estaba comentado ##                traj=expand_dims(traj,2)
## Estaba comentado ##                traj=array(traj,order='Fortran')
## Estaba comentado ##                rango_traj=traj.shape
## Estaba comentado ## 
## Estaba comentado ##        num_parts=rango_traj[0]
## Estaba comentado ##        num_frames=rango_traj[1]
## Estaba comentado ##        if (rango_traj[2]!=dimensions):
## Estaba comentado ##            print '# Error with dimension of traj and ranges'
## Estaba comentado ##            pass
## Estaba comentado ## 
## Estaba comentado ##        len_str=0
## Estaba comentado ##        for aa in ranges:
## Estaba comentado ##            bb=len(str(aa[0]))
## Estaba comentado ##            if (bb>len_str): 
## Estaba comentado ##                len_str=bb 
## Estaba comentado ##            bb=len(str(aa[1]))
## Estaba comentado ##            if (bb>len_str): 
## Estaba comentado ##                len_str=bb 
## Estaba comentado ##                
## Estaba comentado ##        len_str=len_str+1
## Estaba comentado ## 
## Estaba comentado ##        #print num_parts,num_frames,dimensions
## Estaba comentado ##        #print ranges
## Estaba comentado ## 
## Estaba comentado ##        LocalKin.traj2net(len_str,traj,ranges,num_parts,num_frames,dimensions)
## Estaba comentado ##        
## Estaba comentado ## 
## Estaba comentado ##        self.Ts=True
## Estaba comentado ##        self.T_ind=deepcopy(LocalKin.t_ind)
## Estaba comentado ##        self.T_wl=deepcopy(LocalKin.t_tau)
## Estaba comentado ##        self.T_start=deepcopy(LocalKin.t_start)
## Estaba comentado ##        self.build_from_Ts()
## Estaba comentado ##        for ii in range(self.num_nodes):
## Estaba comentado ##            label=str(LocalKin.labels[ii][:])
## Estaba comentado ##            self.node[ii].label=label
## Estaba comentado ##            self.labels[label]=ii
## Estaba comentado ## 
## Estaba comentado ## 
## Estaba comentado ##        LocalKin.free_memory_ts()
## Estaba comentado ##        if verbose:
## Estaba comentado ##            self.info(update=False,verbose=True)
## Estaba comentado ##            pass
## Estaba comentado ##        else:
## Estaba comentado ##            pass
        

    


