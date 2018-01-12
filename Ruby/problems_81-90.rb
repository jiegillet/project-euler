# ! /usr/local/bin/ruby19
##### http://projecteuler.net #####

# #### Problem 81 #####
# m=[]
# File.open("matrix.txt","r").each_line do |line|
#   m<<line.chomp.split(/,/).map{ |x| x.to_i  }
# end
# 
# 1.upto(m.length-1) do |d|
#   m[0][d]+=m[0][d-1]
#   m[d][0]+=m[d-1][0]
#   1.upto(d-1) do |j|
#     m[j][d-j]+=[m[j-1][d-j],m[j][d-j-1]].min
#   end
# end
# (m.length).upto(2*m.length-1) do |d|
#   (d-m.length+1).upto(m.length-1) do |j|
#     m[j][d-j]+=[m[j-1][d-j],m[j][d-j-1]].min
#   end
# end
# puts m.last.last
# # 
# #### Problem 82 #####
# m=[]
# File.open("matrix.txt","r").each_line do |line|
#   m<<line.chomp.split(/,/).map{ |x| x.to_i  }
# end
# 
# p_new=(0..m.length-1).map{ |x| m[x][0]+m[x][1]  }
# 2.upto(m.length-2) do |j|
#   p=p_new.dup
#   0.upto(m.length-1) do |i|
#     s=p[i]
#     0.upto(m.length-1) do |k|
#       s=[p[k]+((k+1)..i).inject(0){|sum,x| sum+m[x][j-1]},s].min if k<=i
#       s=[p[k]+(i..(k-1)).inject(0){|sum,x| sum+m[x][j-1]},s].min if k>i
#     end
#     p_new[i]=s+m[i][j]
#   end
# end
# puts (0..m.length-1).map{ |x| p_new[x]+m[x].last}.min
# 
# #### Problem 83 #####
# m=[]
# File.open("matrix.txt","r").each_line do |line|
#   m << line.chomp.split(/,/).map{ |x| x.to_i  }
# end
# 
# ## Floyd–Warshall algorithm ## Too slow
# nm=m.length
# n=nm**2
# path=[].fill(1.0/0,0..n*n-1) 
# 0.upto(nm-1) do |i|
#   0.upto(nm-1) do |j|
#     path[(i*nm+j)*n+(i*nm+j)]=0
#     path[(i*nm+j)+(i*nm+j-1)*n]=path[(i*nm+j)*n+(i*nm+j-1)]=m[i][j]+m[i][j-1] if j>0
#     path[(j*nm+i)+((j-1)*nm+i)*n]=path[(j*nm+i)*n+((j-1)*nm+i)]=m[j][i]+m[j-1][i] if j>0
#   end
# end
# 
# 0.upto(n-1) do |k|
#   0.upto(n-1) do |i|
#     0.upto(n-1) do |j|
#       path[i*n+j]=[ path[i*n+j],path[i*n+k]+path[k*n+j]].min
#     end
#   end
# end
# puts (path[n-1]+m[0][0]+m.last.last)/2

# ## Dijkstra's algorithm ##
# def dijkstra(source,target,m)
#   n=m.length
#   graph=(0..n**2-1).map{ |x| x }
#   dist,previous,sum=[],[],[]
#   graph.each do |v|
#     dist[v]=1.0/0
#   end
#   dist[source]=m[source/n][source%n]
#   q=graph
#   while q!=[]
#     u,min=0,1.0/0
#     q.each{ |x| u,min=x,dist[x] if dist[x]<min }
#     break if min==1.0/0 || u==target
#     q=q-[u]
#     [u-1,u+1,u+n,u-n].each do |x|
#       next if  (u/n!=x/n && u%n!=x%n) || !q.include?(x)
#       alt=dist[u]+m[x/n][x%n]
#       dist[x],previous[x]=alt,u if alt<dist[x]
#       sum<<alt if x==target
#     end
#   end
#   puts sum.min
#   previous
# end
# 
# previous=dijkstra(0,m.length**2-1,m)
# s,u=[],m.length**2-1
# while previous[u]
#   s.insert(0,u)
#   u=previous[u]
# end
# print s
#
# #### Problem 84 #####
# def shuffle(list)
#   l=list.dup
#   0.upto(l.length-1) do |i|
#     r=i+rand(l.length-i)
#     l[i],l[r]=l[r],l[i]
#   end
#   l
# end
# 
# count={}
# (0..39).map{ |x|  count[x]=0}
# cc=[0,10,nil,nil,nil,nil,nil,nil,nil,nil,nil,nil,nil,nil,nil,nil]
# ch=[0,10,11,24,39,5,"R","R","U","B3",nil,nil,nil,nil,nil,nil]
# 
# 1000.times do
#   cc,ch=shuffle(cc),shuffle(ch)
#   n,n2,dd=0,0,0
#   n_cc,n_ch=0,0
#   10000.times do
#     count[n2]+=1
#     d1,d2=1+rand(4),1+rand(4)
#     d1==d2 ? dd+=1 : dd=0
#     if dd==3
#       n=0
#       next
#     end
#     n=(n2+d1+d2)%40
#     n2=case n
#     when 2,17,33
#       n_cc+=1
#       n=case cc[n_cc%16]
#       when 0,10 then cc[n_cc%16]
#       else n
#       end
#     when 7,22,36
#       n_ch+=1
#       case ch[n_ch%16]
#       when "R"
#         case n
#         when 7 then 15
#         when 22 then 25
#         when 36 then 5
#         end
#       when "U"
#         case n
#         when 7,36 then 12
#         when 22 then 28
#         end
#       when "B3" then n-3
#       when nil then n
#       else ch[n_ch%16]
#       end
#     when 30 then 10
#     else n
#     end
#   end
# end
# print count.sort{ |a,b| b[1] <=> a[1] },"\n"
# 
# #### Problem 85 #####
# min,hm,lm=1.0/0,0,0
# 30.upto(100) do |h|
#   30.upto(100) do |l|
#     n=h*(h+1)*l*(l+1)/4
#     min,hm,lm=2*10**6-n,h,l if (2*10**6-n).abs<min.abs
#   end
# end
# puts "h=#{hm}  l=#{lm}  n=#{min} a=#{lm*hm}" 
#
# ##### Problem 86 #####
# def pyth_triples_cuboid(t,n,m)
#   t=[3,4,5] if t.length==0
#   i=1
#   while i*(t[0]+t[1])<=3*m && (i*t[1]<=m || i*t[0]<=m)
#     a,b=[i*t[0],i*t[1]].min,[i*t[0],i*t[1]].max
#     n+=a/2 if b<=m
#     n+=b/2-(b-a-1) if b/2>=b-a
#     i+=1
#   end
#   t1=[]
#   t1[0]=[t[0]-2*t[1]+2*t[2],2*t[0]-t[1]+2*t[2],2*t[0]-2*t[1]+3*t[2]]
#   t1[1]=[t[0]+2*t[1]+2*t[2],2*t[0]+t[1]+2*t[2],2*t[0]+2*t[1]+3*t[2]]
#   t1[2]=[-t[0]+2*t[1]+2*t[2],-2*t[0]+t[1]+2*t[2],-2*t[0]+2*t[1]+3*t[2]]
#   t1.each{ |t2| n=pyth_triples_cuboid(t2,n,m) if t2[0]+t2[1]<=3*m && (t2[1]<=m || t2[0]<=m)}
#   n
# end
# 
# min,max=1000,5000
# until max-min<=1
#   c=(min+max)/2
#   b=pyth_triples_cuboid([],0,c)
#   if b>10**6
#     max=(min+max)/2
#   else
#     min=(min+max)/2
#   end
# end
# puts "M=#{c} => n=#{pyth_triples_cuboid([],0,c)}"
# 
# ##### Problem 87 #####
# primes = File.open("primes_to_1E+04.txt","r").readline.split(/,/).map{ |x|  x.to_i}
# lim=5E7
# n,l=0,[]
# primes.each do |a|
#   primes.each do |b|
#     primes.each do |c|
#       break if a**2+b**3+c**4 >= lim
#       l<< a**2+b**3+c**4
#     end
#     break if a**2+b**3 >= lim
#   end
#   break if a**2>= lim
# end
# puts l.uniq.length

# ##### Problem 88 #####
# def combinations(l,k,start,c,v,f) 
#   if c >= k
#     f<<v.dup
#     return 
#   end
#   start.upto(l.length-1) do |i|
#     v[c] = l[i]
#     combinations(l,k,i+1,c+1,v,f)
#   end
#   f
# end
# 
def prime_decomp(n,primes)
  d=[]
  primes.each do |p|
    x=[p,0] if n%p==0
    until n%p!=0
      n/=p
      x[1]+=1
    end
    d<<x if x!=nil
    break if p>n
  end
  d
end

def divisors(n)
  d=[]
  2.upto(n/2) do |p|
    n1=n
    until n1%p!=0
      n1/=p
      d<<p
    end
    break if p>n
  end
  d
end
# 
# # primes=File.open("primes_to_1E+04.txt","r").readline.split(/,/).map{ |x|  x.to_i}
# # k=[0,0]
# # 2.upto(50) do |i|
# #   l=divisors(i)
# #   2.upto(l.length) do |j|
# #     f=combinations(l,j,0,0,[],[]).uniq
# #     f.each do |x| 
# #       p=x.inject(1){ |p,x| p*x }
# #       break if p>i
# #       s=x.inject(0){ |s,x| s+x }
# # #     puts "i=#{i}  dec=#{l}  comb=#{x}  p=#{p}  s=#{s}  k=#{p-s+j}"
# #       k[p-s+j]=i if  k[p-s+j]==nil || k[p-s+j]>i
# #     end
# #   end
# # end
# # print k,"\n"
# # print k[28]
# 
# primes=File.open("primes_to_1E+04.txt","r").readline.split(/,/).map{ |x|  x.to_i}
# n=12
# d=[]
# primes.each do |p|
#   break if p*2>n
#   n1=n
#   c=0
#   while n1/p>=1
#     c+=1
#     n1/=p
#   end
#   d<<[p,c]
# end
# 
# k=[0,0]
# c=0
# 2.upto(6) do |j|
#   f=combinations(d,j,0,0,[],[])
#   f.each do |co|
#     pr=(0..j-1).map{ |x| co[x][0] }
#     ex=(0..j-1).map{ |x| co[x][1]+1 }
#     puts "comb=#{co}  pr=#{pr}  ex=#{ex} \n"
#     2.upto(ex.inject(1){|p,x| p*x}-1) do |i|
#       exp=(0..j-1).map{ |k| (i/ex[k+1..j-1].inject(1){|p,x| p*x })%ex[k]}
#       next if (a=exp.inject(0){ |s,x| s+x} )==1
#       
#      # # l=(0..j-1).map{ |k| pr[k]**exp[k]}
#      #  p=l.inject(1){ |p,x| p*x }
#      # # break if p>n
#      #  s=l.inject(0){ |s,x| x>1? s+x : s}
#      p=(0..j-1).inject(1){ |p,x| p*pr[x]**exp[x]}
#      s=(0..j-1).inject(0){ |s,x| exp[x]>0? s+pr[x]*exp[x] : s}
#      puts "exp=#{exp} n=#{(0..j-1).map{ |k| pr[k]**exp[k]}}  a=#{a}  p=#{p}  s=#{s}  k=#{p-s+j}" 
#       c+=1;k[p-s+a]=p if  k[p-s+a]==nil || k[p-s+a]>p
#     end
#   end
# end
# 
# print k,"\n"
# print k[2..n].uniq
# 
# n,k=10,5
# tab=[k].fill(0,1..n-1)
# s=(0..n-1).inject(0){ |s,e| s+ (e+1)*tab[e] }
# p=(0..n-1).inject(1){ |p,e| p* (e+1)**tab[e] }
# p tab
# puts "s=#{s}   p=#{p}"
# while tab[n-1]!=k do
#   if tab[n-1]!=0 then
#     tmp=tab[n-1]
#     tab[n-1]=0
#     i=n-1
#     i=i-1 until tab[i]!=0 
#     tab[i]=tab[i]-1
#     tab[i+1]=tmp+1
#   else
#     i=n-1
#     i=i-1 until tab[i]!=0 
#     tab[i]=tab[i]-1
#     tab[i+1]=1
#   end
#   s=(0..n-1).inject(0){ |s,e| s+ (e+2)*tab[e] }
#   p=(0..n-1).inject(1){ |p,e| p* (e+2)**tab[e] }
#   p tab
#   puts "s=#{s}   p=#{p}   k=#{p-s+k}"
#  # break if s==p
# end
primes=File.open("primes_to_1E+06.txt","r").readline.split(/,/).map{ |x|  x.to_i}
s=0
oldk,kmax=0,0
n=3
until kmax>=12_000 do
  n+=1
  p=prime_decomp(n,primes)
  next if p.length==1 and p[0][1]==1
  smax=p.inject(0){ |smax,e| smax+e[1]  }
  # kmin=n-(n/p[0][0]+p[0][0])+2;
  kmax=n-p.inject(0){ |z,e| z+e[0]*e[1] } + smax
  # puts "n=#{n}   kmin=#{kmin}   kmax=#{kmax}"
  s+=n if kmax>oldk
  oldk=kmax if kmax>oldk
end
p s





























