#! /usr/local/bin/ruby19
##### http://projecteuler.net #####
def gen_primes_to(n)
  primes=[2,3]
  j=2
  while primes.last+j <= n
    x=primes.last+j
    primes.each do |xi|
      if x%xi==0
        j+=2
        break
      elsif x<xi**2
        primes << x
        j=2
        break
      end
    end
  end
  primes
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

# ##### Problem 41 #####
# xmax=0
# 0.upto(2*3*4*5*6*7)do |k|
# x=perm_unsorted(k,[1,2,3,4,5,6,7]).to_s.to_i
# next if x%2==0
# j=1
# prime=true
# (3..Math.sqrt(x).to_i).step(2) do |xi|
#   if x%xi==0
#     prime=false
#     break
#   end
# end
# xmax=[x,xmax].max if prime
# end
# puts xmax
# 
# ##### Problem 42 #####
# f=File.open("words.txt","r").readline.split(/\",\"|\"/)
# t=(1..20).map{ |i| i*(i+1)/2  }
# puts f.inject(0){ |n,fi| if t.include?(fi.split(//).inject(0){ |s,c| s+c.ord-64}) then n+1 else n end }
# 
# ##### Problem 43 #####
# prime=[2,3,5,7,11,13,17]
# dig="0123456789".split(//)
# s,p17=0,0
# 
# until p17>999
#   p17+=17
#   next if p17.to_s.split(//).uniq.length!=3
#   0.upto(fact(7)-1)do |k|
#     x=perm_unsorted(k,dig-p17.to_s.split(//)).join.split(//)+p17.to_s.split(//)
#     d=true
#     1.upto(6) do |i|
#       next if x.values_at(i..i+2).join.to_i%prime[i-1]==0
#       d=false
#       break
#     end
#     s+=x.join.to_i if d
#     puts x.join if d
#   end
# end
# puts "s=#{s}"
# 
# ##### Problem 44 #####
# def penta(n)
#   n*(3*n-1)/2
# end
# 
# 1.upto(3000) do |i|
#   (i+1).upto(3000) do |j|
#     pi,pj=penta(i),penta(j)
#     if ((1+Math.sqrt(24*(pj-pi)+1))/6)%1==0
#       puts pj-pi if ((1+Math.sqrt(24*(pj+pi)+1))/6)%1==0
#     end
#   end
# end
# 
# ##### Problem 45 #####
# n=Math.sqrt(8*40755+1).to_i
# n=166
# until false
#   i=n*(3*n-1)/2
#   s1=Math.sqrt(8*i+1).to_i
#   s2=Math.sqrt(24*i+1).to_i
#   break if s1**2==8*i+1 and s2**2==24*i+1 and (1+s2)%6==0  and (s1+1)%4==0
#   n+=1
# end
# puts i
# 
# ##### Problem 46 #####
# lim=10000
# primes=gen_primes_to(lim)
# puts "Primes ready"
# i=1
# ok=false
# until ok
#   i+=2
#   ok=true
#   k=0
#   while 2*k**2<=i-2 do
#     ok=false if primes.include?(i-2*k**2)
#     k+=1
#   end
# end
# puts i
# 
# ##### Problem 47 #####
# primes=gen_primes_to(1000)
# class Integer
#   def prime_factors(primes)
#     n=self
#     p=[]
#     primes.each do |x|
#       if n%x==0
#         p<<x
#         until n%x !=0
#           n/=x
#         end
#         break if n==1
#       end
#     end
#     p
#   end
# end
# 
# i=1
# n=4
# p1=[]
# ok=false
# until ok
#   p2,p3,p4=[],[],[]
#   p2=(i+1).prime_factors(primes)
#   if (p1-p2).length<n or (p2-p1).length<n
#     p1=p2
#     i+=1
#     next
#   end
#   p3=(i+2).prime_factors(primes)
#   if (p3-p2).length<n or (p2-p3).length<n
#     p1=p3
#     i+=2
#     next
#   end
#   p4=(i+3).prime_factors(primes)
#   if (p3-p4).length<n or (p4-p3).length<n
#     p1=p4
#     i+=3
#     next
#   end
#   ok=true
# end
# puts i
# 
# ##### Problem 48 #####
# puts (1..1000).inject{ |s,i| s+i**i }%10**10
# 
# ##### Problem 49 #####
# p=gen_primes_to(9999).select{|x| x.to_s.length==4}
# c=0
# 0.upto(p.length-3) do |i|
#   (i+1).upto(p.length-2) do |j|
#     s=p[i].to_s.split(//).sort.to_s
#     if s==p[j].to_s.split(//).sort.to_s
#       if p.include?(2*p[j]-p[i]) and s==(2*p[j]-p[i]).to_s.split(//).sort.to_s
#           puts p[i].to_s<<p[j].to_s<<(2*p[j]-p[i]).to_s
#           c+=1
#           break if c==2
#       end  
#     end
#   end
#   break if c==2
# end
# 
# ##### Problem 50 #####
# lim=10**6
# p=gen_primes_to(lim-1)
# puts "Primes generated"
# s,l=0,0
# while s+p[l]<lim
#   s+=p[l]
#   l+=1
# end
# imin=0
# n=0
# until p.include?(s)
#   n+=1
#   0.upto(n) do |i|
#     s=(i..(i+l-n-1)).inject(0){ |sum,x| sum+p[x]  }
#     break if p.include?(s)
#   end
# end
# puts "The prime #{s} is the sum of #{l-n} primes."