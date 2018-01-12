#! /usr/local/bin/ruby19
##### http://projecteuler.net #####
def gen_primes_to(n)
  f=File.open("primes_to_#{'%.0E' % n}.txt","w")
  primes=[2,3]
  f.write("2,3")
  j=2
  while primes.last+j <= n
    x=primes.last+j
    primes.each do |xi|
      if x%xi==0
        j+=2
        break
      elsif x<xi**2
        primes << x
        f.write(",#{x}")
        j=2
        break
      end
    end
  end
  f.close
end

def perm_unsorted(k,l)
  s=l.dup
  2.upto(s.length){ |j|  
    s[k%j],s[j-1]=s[j-1],s[k%j]
    k/=j
  }
  s
end

def fact(n)
  return 1 if n<2
  (2..n).inject(1){ |p,i| p*i }
end

# ##### Problem 61 #####
# def cycle?(a,b)
#   a%100==b/100
# end
# 
# p=[[],[],[],[],[],[]]
# 1.upto(150) do |n|  
#   p[0]<<n*(n+1)/2 if (n*(n+1)/2).to_s.length==4
#   p[1]<<n**2 if (n**2).to_s.length==4
#   p[2]<<n*(3*n-1)/2 if (n*(3*n-1)/2).to_s.length==4
#   p[3]<<n*(2*n-1) if (n*(2*n-1)).to_s.length==4
#   p[4]<<n*(5*n-3)/2 if (n*(5*n-3)/2).to_s.length==4
#   p[5]<<n*(3*n-2) if (n*(3*n-2)).to_s.length==4
# end
# 
# def find_cycle(p)
#   0.upto(fact(5)-1) do |o|
#     n=perm_unsorted(o,[0,1,2,3,4])
#     0.upto(p[5].length-1) do |i|
#       p1=p[n[0]].select{ |x| cycle?(p[5][i],x) }
#       0.upto(p1.length-1) do |j|
#         p2=p[n[1]].select{ |x|  cycle?(p1[j],x) }
#         0.upto(p2.length-1) do |k|
#           p3=p[n[2]].select{ |x|  cycle?(p2[k],x) }
#           0.upto(p3.length-1) do |l|
#             p4=p[n[3]].select{ |x|  cycle?(p3[l],x) }
#             0.upto(p4.length-1) do |m|
#               p5=p[n[4]].select{ |x|  cycle?(p4[m],x) }
#               0.upto(p5.length-1) do |q|
#                 return [p[5][i],p1[j],p2[k],p3[l],p4[m],p5[q]] if cycle?(p5[q],p[5][i])
#               end
#             end
#           end
#         end
#       end
#     end
#   end
# end
# x=find_cycle(p)
# print x,"\n"
# puts x.inject(0){ |s,y| s+y  } 
# 
# ##### Problem 62 #####
# cube=(1..10000).map{ |i| (i**3).to_s.split(//).sort.join  }
# 
# 0.upto(cube.length-1) do |i|
#   c=cube[i]
#   n=0
#   (i+1).upto(cube.length-1) do |j|
#     n+=1 if c==cube[j]
#     break if c.length<cube[j].length
#   end
#   if n==4
#     puts (i+1)**3
#     break
#   end
# end
# 
# ##### Problem 63 #####
# c=1
# 2.upto(9) do |i|
#   n=1
#   while (i**n).to_s.length == n
#     c+=1
#     n+=1
#   end
# end
# puts c
# 
# ##### Problem 64 #####
# t=0
# 1.upto(10000){ |n|
#   sq=Math.sqrt(n)
#   next if (sq.to_i)**2==n
#   c=0
#   a={}
#   x,y=sq.to_i,1
#   until a["#{x} #{y}"]!=nil
#     a["#{x} #{y}"]=c
#     d=(y*(sq+x)/(n-x**2)).to_i
#     x,y=-x+d*(n-x**2)/y,(n-x**2)/y
#     c+=1
#   end
#   t+=(c-a["#{x} #{y}"])%2
# }
# puts t
# 
# ##### Problem 65 #####
# def seq_e(n)
#   return 2 if n==1
#   return 2*n/3 if n%3==0
#   1
# end
# # Code missing!!!
# puts n.to_s.split(//).inject(0){ |s,x| s+x.to_i  }
# 
# ##### Problem 66 #####
# # http://www.alpertron.com.ar/METHODS.HTM
# def convergent(a)
#   n,d=a.last,1
#   (a.length-2).downto(0){|i| n,d=n*a[i]+d,n }
#   [n,d]
# end
# 
# m,md=0,0
# 2.upto(1000) do |d|
#   sq=Math.sqrt(d)
#   next if sq%1==0
#   x=sq.to_i
#   a=[x,((sq+x)/(d-x**2)).to_i]
#   x,y=-x+a[1]*(d-x**2),(d-x**2)
#   l=convergent(a)
#   i=2
#   until l[0]**2 - d*l[1]**2==1 
#     a[i]=(y*(sq+x)/(d-x**2)).to_i
#     x,y=-x+a[i]*(d-x**2)/y,(d-x**2)/y
#     i+=1
#     l=convergent(a)
#   end
#   md,m=d,l[0] if m<l[0]
# end
# puts md
# 
# ##### Problem 67 #####
# f=File.open("triangle.txt","r")
# t=[]
# while line = f.gets 
#   t << line.split(" ") 
# end
# (t.length-2).downto(0) do |i|
#   0.upto(i) do |j|
#     t[i][j]=t[i][j].to_i+[t[i+1][j],t[i+1][j+1]].max.to_i
#   end
# end
# print t[0][0]
# 
# ##### Problem 68 #####
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
# d=10
# v=(1..d).map{ |i| i }
# x=combinations(v,d/2,0,0,[],[])
# 
# m=""
# x.each do |i|
#   v2=v-i
#   s=[v2[0],i[4],i[2],v2[4],i[2],i[0],v2[3],i[0],i[3],v2[2],i[3],i[1],v2[1],i[1],i[4]].join
#   m=[m,s].max if (2*i.inject(0){|s,x|s+x}+v2.inject(0){|s,x|s+x})%(d/2)==0 && s.length==16
# end
# puts m
# 
# ##### Problem 69 #####
# f=File.open("primes_to_1E+06.txt","r")
# primes = f.readline.split(/,/)
# f.close
# 
# m,nm,lim=0,0,10**6
# sieve=[].fill(false,0..lim)
# primes.each{ |x| sieve[x.to_i]=true}
# p=[0,1]
# 2.upto(lim) do |n|
#   if sieve[n]
#     p[n]=n-1
#   else
#     i=0
#     i+=1 until n%primes[i].to_i==0
#     pi=primes[i].to_i
#     n/pi%pi==0 ? p[n]=p[n/pi]*pi : p[n]=p[n/pi]*(pi-1)
#   end
#   m,nm=n/p[n],n if n/p[n] > m
# end
# puts nm
# 
# ##### Problem 70 #####
# primes = File.open("primes_to_1E+07.txt","r").readline.split(/,/).map{ |x| x.to_i  }
# 
# # def prime_decomp_to(lim,primes)
# #   sieve=[].fill(false,0..lim)
# #   primes.each{ |x| sieve[x.to_i]=true}
# #   d=[[],[1]]
# #   2.upto(lim) do |n|
# #     if sieve[n]
# #       d<<[1,n]
# #     else
# #       i=0
# #       i+=1 until n%primes[i].to_i==0
# #       d<<(d[n/primes[i].to_i]+[primes[i].to_i]).uniq
# #     end
# #   end
# #   d
# # end
# 
# m,nm,lim=100,0,10**7
# sieve=[].fill(false,0..lim)
# primes.each{ |x| sieve[x]=true}
# p=[0,1.0]
# 2.upto(lim) do |n|
#   if sieve[n]
#     p[n]=n-1
#   else
#     i=0
#     i+=1 until n%primes[i]==0
#     pi=primes[i]
#     n/pi%pi==0 ? p[n]=p[n/pi]*pi : p[n]=p[n/pi]*(pi-1)
#   end
#   m,nm=n.to_f/p[n],n if n.to_s.split(//).sort == p[n].to_i.to_s.split(//).sort if n.to_f/p[n] < m
# end
# puts "n=#{nm}   phi=#{p[nm]}   m=#{m}"