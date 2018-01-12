#! /usr/local/bin/ruby19
##### http://projecteuler.net #####

def fact(n)
  return 1 if n<2
  (2..n).inject(1){ |p,i| p*i }
end

# ##### Problem 71 #####
# def gcd(a,b)
#   a,b=b,a%b while b!=0
#   a
# end
# 
# f=[]
# lim=10**6
# 2.upto(lim) do |d|
#  (0.4285714285*d).to_i.upto(d) do |n|
#   break if n/d.to_f >= 3/7.0
#   f<<"#{n}/#{d}"
#  end 
# end
# m="0"
# f.each{ |x| m=x if  eval(x+".0")>eval(m+".0") }
# puts m.split('/')[0].to_i/gcd(m.split('/')[0].to_i,m.split('/')[1].to_i)
# 
# ##### Problem 72 #####
# f=File.open("primes_to_1E+06.txt","r")
# primes = f.readline.split(/,/)
# f.close
# 
# m,nm,lim=100,0,10**6
# sieve=[].fill(false,0..lim)
# primes.each{ |x| sieve[x.to_i]=true}
# p=[0,1]
# s=0
# 2.upto(lim) do |n|
#   if sieve[n]
#     p[n]=n-1
#   else
#     i=0
#     i+=1 until n%primes[i].to_i==0
#     pi=primes[i].to_i
#     n/pi%pi==0 ? p[n]=p[n/pi]*pi : p[n]=p[n/pi]*(pi-1)
#   end
#   s+=p[n]
# end
# puts s
# 
# ##### Problem 73 #####
# def gcd(a,b)
#   a,b=b,a%b while b!=0
#   a
# end
# 
# f=[]
# lim=10**4
# 3.upto(lim) do |d|
#  (1/3.0*d +1 ).to_i.upto((0.5*d).to_i) do |n|
#   break if n/d.to_f >= 0.5
#   g=gcd(n,d)
#   f<<"#{n/g}/#{d/g}" if  g==1
#  end 
# end
# print f.length
# 
# ##### Problem 74 #####
# f=(0..9).map{ |x| fact(x)}
# n60=0
# path={}
# 0.upto(10**6-1) do |n|
#   next if path[n.to_s]!=nil
#   c,x=0,n.to_s
#   l={}
#   l2=[x]
#   until l[x]!=nil
#     l[x]=1
#     x=x.split(//).inject(0){ |s,x| s+f[x.to_i]}.to_s
#     c+=1
#     l2[c]=x.to_s
#     if path[x]!=nil
#       c+=path[x]
#       break
#     end
#   end
#   (l2.length-1).downto(0){ |i| path[l2[i]]=c-i }
#   n60+=1 if c == 60
# end
# puts n60
# 
# ##### Problem 75 #####
# def pyth_triples_perim(t,perim,lim)
#   t1,p=[],[]
#   t1[0]=[t[0]-2*t[1]+2*t[2],2*t[0]-t[1]+2*t[2],2*t[0]-2*t[1]+3*t[2]]
#   t1[1]=[t[0]+2*t[1]+2*t[2],2*t[0]+t[1]+2*t[2],2*t[0]+2*t[1]+3*t[2]]
#   t1[2]=[-t[0]+2*t[1]+2*t[2],-2*t[0]+t[1]+2*t[2],-2*t[0]+2*t[1]+3*t[2]]
#   t1.each{ |t2| p+=[t2[0]+t2[1]+t2[2]] if t2[0]+t2[1]+t2[2] <= lim}
#   perim+=p
#   t1.each{ |t2| p+=pyth_triples_perim(t2,perim,lim) if t2[0]+t2[1]+t2[2] <= lim}
#   p
# end
# 
# t,perim,lim=[3,4,5],[12],2E6
# perim+=pyth_triples_perim(t,perim,lim)
# l=[].fill(0,0..lim)
# perim.each do |p|
#   x=p
#   while x<=lim
#     l[x]+=1
#     x+=p
#   end
# end
# c=0
# puts l.select{ |x| x==1  }.length
# 
# ##### Problem 76 #####
# tot = 100
# ways=[1]
# 1.upto(tot) do |coin|
#   coin.upto(tot) do |j|
#     ways[j] = ways[j].to_i + ways[j - coin]
#   end
# end
# puts ways[tot]
# 
# ##### Problem 77 #####
# primes = File.open("primes_to_1E+04.txt","r").base_exp.txte.split(/,/)
# f.close
# tot = 1
# ways=[0,0]
# 
# until ways[tot]>=5000
#   tot+=1
#   ways=[1]+[].fill(0,0..tot)
#   primes.each do |p|
#     break if p.to_i>tot
#     p.to_i.upto(tot) do |j|
#       ways[j] += ways[j - p.to_i]
#     end
#   end
# end
# puts tot
# 
# #### Problem 78 #####
# p=[1,1]
# n=2
# until p[n-1]%10**6==0
#   s=0
#   pm=1
#   1.upto(n) do |k|
#     k*(3*k-1)/2<=n ? s1=p[n-k*(3*k-1)/2] : s1=0
#     k*(3*k+1)/2<=n ? s2=p[n-k*(3*k+1)/2] : s2=0
#     s+=(s1+s2)*pm
#     pm*=-1
#     break if s1+s2==0
#   end
#   p[n]=s
#   n+=1
# end
# puts "#{n-1} => #{p[n-1]}"
# 
# #### Problem 79 #####
# f=File.open("keylog.txt","r")
# n,code=0,[]
# while line = f.gets
#   code[n]=line.chomp.split(//)
#   n+=1
# end
# 
# l={}
# code.each do |c|
#   l[c[0]]=(l[c[0]].to_s+c[1]+c[2]).split(//).uniq.join
#   l[c[1]]=(l[c[1]].to_s+c[2]).split(//).uniq.join
#   l[c[2]]=(l[c[2]].to_s).split(//).uniq.join
# end
# x=l.sort{ |a,b| b.to_s.length <=> a.to_s.length }
# 0.upto(x.length-1) do |i|
#   print x[i][0]
# end
# 
# #### Problem 80 #####
# def convergent(a)
#   n,d=a.last,1
#   (a.length-2).downto(0){|i| n,d=n*a[i]+d,n }
#   [n,d]
# end
# 
# sum=0
# 2.upto(100) do |d|
#   sq=Math.sqrt(d)
#   next if sq%1==0
#   x=sq.to_i
#   a=[x,((sq+x)/(d-x**2)).to_i]
#   x,y=-x+a[1]*(d-x**2),(d-x**2)
#   l=convergent(a)
#   oldl=[1,47]
#   i=2
#   until ((oldl[0]%oldl[1])*10**100)/oldl[1] == ((l[0]%l[1])*10**100)/l[1]
#     oldl=l
#     a[i]=(y*(sq+x)/(d-x**2)).to_i
#     x,y=-x+a[i]*(d-x**2)/y,(d-x**2)/y
#     i+=1
#     l=convergent(a)
#   end
#   sum+=(l[0]*10**100/l[1]).to_s.split(//)[0..99].inject(0){ |s,e| s+e.to_i}
# end
# puts sum