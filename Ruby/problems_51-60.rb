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
#
# ##### Problem 51 #####
# f=File.open("primes.txt","r")
# primes = f.readline.split(/,/)
# sieve=[]
# sieve.fill(false,0..10**6)
# primes.each{ |x| sieve[x.to_i]=true}
#
# primes.each do |prime|
#   cmax=0
#   prime.split(//).uniq.each do |d|
#     c=0
#     0.upto(9) do |i|
#       p=prime.gsub(d,i.to_s).to_i
#       c+=1  if sieve[p] and p.to_s.length==prime.length
#     end
#     cmax=[c,cmax].max
#   end
#   if cmax>7
#     puts prime
#     break
#   end
# end
#
# ##### Problem 52 #####
# n=1
# n+=1 until (1..5).inject(true){ |r,i|  r and (((i+1)*n).to_s.split(//).sort==n.to_s.split(//).sort) }
# puts n
#
# ##### Problem 53 #####
# cpt=0
# n,m=100,100
# c=[[1],[]]
# k=1
# (1..n).each{ |i|
#   c[k][0],c[k][i]=1,1
#   (1..[m,i-1].min).each{ |j|
#     c[k][j]=c[1-k][j-1]+c[1-k][j]
#     cpt+=1 if c[k][j]>10**6
#     }
#   k=1-k
# }
# puts cpt
#
# ##### Problem 54 #####
# def p1_wins?(p1,p2)
#   p=[p1,p2]
#   val,suit=[[],[]],[[],[]]
#   0.upto(1) do |j|
#     0.upto(4) do |i|
#       val[j][i],suit[j][i]=p[j][i].split(//)
#       val[j][i].tr!("TJQKA","LMNOP")
#     end
#   end
#   # Royal Flush
#   if /A([HDCS])J\1K\1Q\1T/=~p1.sort.join or /A([HDCS])J\1K\1Q\1T/=~p2.sort.join
#     (p1.sort.join=~/A([HDCS])J\1K\1Q\1T/) != nil
#   # Straight Flush
#   elsif ca=(suit[0].uniq.length==1 and "23456789LMNOP".include?(val[0].sort.join)) or cb=(suit[1].uniq.length==1 and "23456789LMNOP".include?(val[1].sort.join))
#     cb=(suit[1].uniq.length==1 and "23456789LMNOP".include?(val[1].sort.join))
#     return val[0].sort.last > val[1].sort.last if ca and cb
#     ca
#   # Four of a Kind
#   elsif /(?<one>[2-9L-P])(\k<one>){3}/=~val[0].join or /(?<two>[2-9L-P])(\k<two>){3}/=~val[1].join
#     /(?<two>[2-9L-P])(\k<two>){3}/=~val[1].join
#     one.to_s > two.to_s
#   # Full House
#   elsif  /((?<a1>\w)\k<a1>{2}(?<b1>\w)\k<b1>)|(?<c1>\w)\k<c1>(?<d1>\w)\k<d1>{2}/=~val[0].sort.join or /((?<a2>\w)\k<a2>{2}(?<b2>\w)\k<b2>)|(?<c2>\w)\k<c2>(?<d2>\w)\k<d2>{2}/=~val[1].sort.join
#     /((?<a2>\w)\k<a2>{2}(?<b2>\w)\k<b2>)|(?<c2>\w)\k<c2>(?<d2>\w)\k<d2>{2}/=~val[1].sort.join
#     [a1.to_s,d1.to_s].max > [a2.to_s,d2.to_s].max
#   # Flush
#   elsif suit[0].uniq.length==1 or suit[1].uniq.length==1
#     if suit[0].uniq.length==1 and suit[1].uniq.length==1
#       k=4
#       k-=1 until val[0].sort[k] != val[1].sort[k]
#       return val[0].sort[k] > val[1].sort[k]
#     end
#     suit[0].uniq.length==1
#   # Straight
#   elsif "23456789LMNOP".include?(val[0].sort.join) or "23456789LMNOP".include?(val[1].sort.join)
#     if "23456789LMNOP".include?(val[0].sort.join) and "23456789LMNOP".include?(val[1].sort.join)
#      return val[0].sort.last > val[1].sort.last
#     end
#     "23456789LMNOP".include?(val[0].sort.join)
#   # Three of a Kind
#   elsif  /\w?\w?(?<a1>\w)\k<a1>{2}\w?\w?/=~val[0].sort.join or /\w?\w?(?<a2>\w)\k<a2>{2}\w?\w?/=~val[1].sort.join
#     /(?<a2>\w)\k<a2>{2}/=~val[1].sort.join
#     a1.to_s > a2.to_s
#   # Two pairs
#   elsif  /\w?(?<a1>\w)\k<a1>\w?(?<b1>\w)\k<b1>\w?/=~val[0].sort.join or /\w?(?<a2>\w)\k<a2>\w?(?<b2>\w)\k<b2>\w?/=~val[1].sort.join
#     /\w?(?<a2>\w)\k<a2>\w?(?<b2>\w)\k<b2>\w?/=~val[1].sort.join
#     if [a1.to_s,b1.to_s].max == [a2.to_s,b2.to_s].max
#       return [val[0]-[a1]-[b1]]>[val[0]-[a2]-[b2]]  if [a1,b1].min == [a2,b2].min
#       return [a1.to_s,b1.to_s].min > [a2.to_s,b2.to_s].min
#     end
#     [a1.to_s,b1.to_s].max > [a2.to_s,b2.to_s].max
#   # One Pair
#   elsif  /(?<a1>\w)\k<a1>/=~val[0].sort.join or /(?<a2>\w)\k<a2>/=~val[1].sort.join
#    /(?<a2>\w)\k<a2>/=~val[1].sort.join
#    if a1==a2
#      k=4
#      k-=1 until val[0].sort[k] != val[1].sort[k]
#      return val[0].sort[k] > val[1].sort[k]
#    end
#   a1.to_s > a2.to_s
#   # High Card
#   else
#     k=4
#     k-=1 until val[0].sort[k] != val[1].sort[k]
#     val[0].sort[k] > val[1].sort[k]
#   end
# end
#
# f=File.open("poker.txt","r")
# n,tot=0,0
# while line = f.gets
#   i=0
#   p1=line.split(" ").select{ |x|  (i+=1)<6}
#   p2=line.split(" ").select{ |x|  (i-=1)<5}
#   tot+=1
#   n+=1 if p1_wins?(p1,p2)
# end
# f.close
# puts "Player 1 wins #{n} times, player 2 wins #{tot-n} times."
#
# ##### Problem 55 #####
# def Lychrel?(n)
#   50.times{
#   n+=n.to_s.reverse.to_i
#   return false if n==n.to_s.reverse.to_i
#   }
#   true
# end
#
# n=0
# 1.upto(1e4) { |i| n+=1 if Lychrel?(i) }
# puts n
#
# ##### Problem 56 #####
# m=0
# 1.upto(99) do |a|
#   1.upto(99) do |b|
#     m=[m,(a**b).to_s.split(//).inject(0){ |s,d|s+d.to_i}].max
#   end
# end
# puts m
#
# ##### Problem 57 #####
# num,den,n=1,2,0
# 999.times do
#   num,den=den,2*den+num
#   n+=1 if (num+den).to_s.length > den.to_s.length
# end
# puts n
#
# ##### Problem 58 #####
# f=File.open("primes_to_1E8.txt","r")
# primes = f.readline.split(/,/)
# f.close
# sieve=[]
# sieve.fill(false,0..10**8)
# primes.each{ |x| sieve[x.to_i]=true}
#
# num,n,p,t=9,4,3,5
# until p.to_f/t.to_f < 0.1
#   t+=4
#   4.times do
#     num+=n
#     if num<1E8
#       p+=1 if sieve[num]
#     else
#       primes.each do |xi|
#          if num%xi.to_i==0
#            break
#          elsif num<xi.to_i**2
#            p+=1
#            break
#          end
#        end
#     end
#   end
# # puts "num=#{num}   n=#{n+1}   t=#{t}   p=#{p}   #{p.to_f/t.to_f}"
#   n+=2
# end
# puts n-1
#
# ##### Problem 59 #####
# f=File.open("cipher1.txt","r")
# cipher = f.readline.split(/,/).map{ |x| x.to_i  }
# f.close
#
# s1,s2,s3={},{},{}
# (0..cipher.length).step(3) do |i|
#   s1[cipher[i]]=(s1[cipher[i]] || 0)+1
#   s2[cipher[i+1]]=(s2[cipher[i+1]] || 0)+1
#   s3[cipher[i+2]]=(s3[cipher[i+2]] || 0)+1
# end
#
# a1=s1.key(s1.values.max)^(' '.ord)
# a2=s2.key(s2.values.max)^(' '.ord)
# a3=s3.key(s3.values.max)^(' '.ord)
#
# r=0
# (0..cipher.length-1).step(3) do |i|
#   print (cipher[i]^a1).chr
#   print (cipher[i+1]^a2).chr if i+1<cipher.length
#   print (cipher[i+2]^a3).chr if i+2<cipher.length
#   r+=cipher[i]^a1
#   r+=cipher[i+1]^a2 if i+1<cipher.length
#   r+=cipher[i+2]^a3 if i+2<cipher.length
# end
# puts "\n",r
#
# ##### Problem 60 #####
primes = File.open("primes_to_1E+04.txt","r").readline.split(/,/).map{ |x|x.to_i  }
sieve=[].fill(false,0..10**4)
primes.each{ |x| sieve[x]=true}

def prime?(x,primes,sieve)
  return sieve[x] if x<10**4
  primes.each do |xi|
    if x%xi==0
      return false
    elsif x<xi**2
      return true
    end
  end
end

lim,t,list=1000,false,[]
min=10**8

1.upto(primes.length) do |i|
  p1=primes.select{ |p| (prime?("#{primes[i]}#{p}".to_i,primes,sieve) and prime?("#{p}#{primes[i]}".to_i,primes,sieve)) }
  next if p1.length<4
  0.upto(p1.length-1) do |j|
    next if p1[j]==primes[i]
    p2=p1.select{ |p| (prime?("#{p1[j]}#{p}".to_i,primes,sieve) and prime?("#{p}#{p1[j]}".to_i,primes,sieve) and p.to_i>p1[j])  }
    next if p2.length<3
    0.upto(p2.length-1) do |k|
      break if primes[i]+p1[j]+2*p2[k]>min
      p3=p2.select{ |p| (prime?("#{p2[k]}#{p}".to_i,primes,sieve) and prime?("#{p}#{p2[k]}".to_i,primes,sieve) and p.to_i>p2[k]) }
      next if p3.length<2
      0.upto(p3.length-1) do |l|
        break if primes[i]+p1[j]+p2[k]+2*p3[l]>min
        p4=p3.select{ |p| (prime?("#{p3[l]}#{p}".to_i,primes,sieve) and prime?("#{p}#{p3[l]}".to_i,primes,sieve) and p.to_i>p3[l]) }
        if p4.length>0
          t=true
          print list=[primes[i],p1[j],p2[k],p3[l],p4[0]]," sum=",list.inject(0){ |s,e| s+e.to_i}
          min=[list.inject(0){ |s,e| s+e},min].min
          break
        end
      end
      break if t
    end
    break if t
  end
  break if t
end

# print f= "e".split(//).map{ |x| x.ord.to_s(2) }
# print g="r".split(//).map{ |x| x.ord.to_s(2) }
# puts
# 0.upto(0) do |i|
#   puts (f[i].to_i(2)^g[i].to_i(2)).to_s(16)
# end
# puts (64+14).chr
# puts (64+15).chr
# puts (64+21).chr
# puts (64+22).chr
# puts (64+5).chr
# puts (64+1).chr
# puts (64+21).chr
