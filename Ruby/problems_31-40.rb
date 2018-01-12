# ##### http://projecteuler.net #####
# 
# ##### Problem 31 #####
# def number_comb(tot,coins)
#   num={}
#   n=0
#   coins.each{ |val| num[val]=tot/val }
#   indst=coins.length
#   ok=false
#   until ok
#     sum=0
#     indst-=1 until num[coins[indst-1]]>0
#     ind=indst
#     until sum==tot
#         ind-=1
#         num[coins[ind]]=[num[coins[ind]],(tot-sum)/coins[ind]].min
#         x=ind if ind>0 && num[coins[ind]]>0
#         sum+=num[coins[ind]]*coins[ind]
#     end
#     n+=1
#     num[coins[x]]-=1
#     (x-1).downto(0){ |i|  num[coins[i]]=tot/coins[i] }    
#     ok=true
#     1.upto(coins.length-1){ |i| if num[coins[i]]>0 
#        ok=false
#        break
#       end}
#   end
#   n+1
# end
# 
# coins=[1,2,5,10,20,50,100,200]
# puts number_comb(200,coins)

# # With recursion
# def number_comb(tot,coins,coinmax)
#   return 1 if coinmax==coins.length-1
#   s=0
#   coinmax.upto(coins.length-1) do |i|
#     s+=1 if tot==coins[i]
#     s+=number_comb(tot-coins[i],coins,i)  if tot>coins[i]
#   end
#   s
# end
# 
# coins=[200, 100, 50, 20, 10, 5,2,1]
# puts number_comb(200,coins,0)

# ##### Problem 32 #####
# def fact(n)
#   return 1 if n<2
#   (2..n).inject(1){ |p,i| p*i }
# end
# 
# def perm_unsorted(k,l)
#   s=l.dup
#   2.upto(s.length){ |j|  
#     s[k%j],s[j-1]=s[j-1],s[k%j]
#     k/=j
#   }
#   s
# end
# 
# def perm(k,l)
#   s=l.dup
#   n=s.length
#   f=fact(n-1)
#   1.upto(n-1){ |j| 
#     tempj=(k/f)%(n+1-j)
#     temps=s[j+tempj-1]
#     (j+tempj).downto(j+1){ |i| s[i-1]=s[i-2]  }
#     s[j-1]=temps
#     f/=n-j
#   }
#   s
# end
# 
# l=9
# n=[]
# 
# 1.upto(l){ |i| n<<i  }
# 
# s=[]
# 0.upto(fact(l)-1){ |i|
#   p=perm_unsorted(i,n).dup
#   a=p.values_at(0).to_s.to_i
#   b=p.values_at(1..4).to_s.to_i
#   c=p.values_at(5..l-1).to_s.to_i
#   puts "#{a}*#{b}=#{c} is #{a*b==c}" if a*b==c
#   s+=[c] if a*b==c
#   a=p.values_at(0..1).to_s.to_i
#   b=p.values_at(2..4).to_s.to_i
#   c=p.values_at(5..l-1).to_s.to_i
#   puts "#{a}*#{b}=#{c} is #{a*b==c}" if a*b==c
#   s+=[c] if a*b==c
#   }
# puts "Done! The sum is #{s.uniq.inject(0){ |sum,i| sum+i }}"
# 
# ##### Problem 33 #####
# require 'mathn.rb'
# d,n=1,1
# 1.upto(9) do |a|
#   (a+1).upto(9) do |b|
#     1.upto(9) do |c|
#       if c!=a and c!=b and (a*(b*10+c)==(a*10+c)*b or b*(c*10+a)==(b*10+c)*a or b*(a*10+c)==(c*10+b)*a)
#         n*=a
#         d*=b
#       end
#     end
#   end
# end
# puts d/(d.gcd(n))
# 
# ##### Problem 34 #####
# fact=[1]
# 1.upto(9) do |i|
#   fact[i]=i*fact[i-1]
# end
# 
# sum=0
# 3.upto(100000) do |i|
#   if i==i.to_s.split(//).inject(0){ |s,k| s+fact[k.to_i]  }
#     puts i
#     sum+=i
#   end
# end
# puts "The sum is #{sum}"
# 
# ##### Problem 35 #####
# lim=1_000_000-1
# sqlim=Math.sqrt(lim).to_i
# sieve=[true,true,true,true]
# sieve.fill(false,4..(lim-1))
# 1.upto(sqlim){ |x|
#   1.upto(sqlim){ |y|
#     n=4*x**2+y**2
#     sieve[n]=!sieve[n] if n <= lim && (n%12==1 or n%12==5)
#     n=3*x**2+y**2
#     sieve[n]=!sieve[n] if n <= lim && n%12==7
#     n=3*x**2-y**2
#     sieve[n]=!sieve[n] if x>y && n <= lim && n%12==11
#   }
# }
# 3.upto(sqlim){ |i|
#  if sieve[i] then
#    k=1
#    while k*i**2<lim
#      sieve[k*i**2]=false
#      k+=1
#    end
#  end
# }
# sieve[0],sieve[1]=false,false
# 
# def cycl_prime?(i,sieve)
#   a=i.to_s.split(//)
#   return false if a.inject(false){ |p,k| p || k.to_i%2==0  }
#   (1..a.length).inject(true){ |p,k| 
#     p && sieve[(a<<a.shift).to_s.to_i]
#     }
# end
# 
# n=1
# i=3
# until i>=999_999 
#   n+=1 if i%5!=0 and cycl_prime?(i,sieve)
#   i+=2
# end
# puts n+1
# 
# ##### Problem 36 #####
# n=0
# i=1
# until i>=999_999
#   if i.to_s==i.to_s.reverse and i.to_s(2)==i.to_s(2).reverse
#     n+=i
#     puts "#{i}   #{i.to_s(2)}"
#   end
#   i+=2
# end
# puts n
# 
# ##### Problem 37 #####
# def sieve_up(lim)
#   sqlim=Math.sqrt(lim).to_i
#   sieve=[true,true,true,true]
#   sieve.fill(false,4..(lim-1))
#   1.upto(sqlim){ |x|
#     1.upto(sqlim){ |y|
#       n=4*x**2+y**2
#       sieve[n]=!sieve[n] if n <= lim && (n%12==1 or n%12==5)
#       n=3*x**2+y**2
#       sieve[n]=!sieve[n] if n <= lim && n%12==7
#       n=3*x**2-y**2
#       sieve[n]=!sieve[n] if x>y && n <= lim && n%12==11
#     }
#   }
#   3.upto(sqlim){ |i|
#     if sieve[i] then
#       k=1
#       while k*i**2<lim
#         sieve[k*i**2]=false
#         k+=1
#       end
#     end
#   }
#   sieve[0],sieve[1]=false,false
#   sieve
# end
# 
# def trunc_prime?(i,sieve)
#   a=i.to_s.split(//)
#   (0..Math.log10(i).to_i).inject(true){ |p,k|
#     a.shift if k>0
#     (p and sieve[a.to_s.to_i] and sieve[i/10**k]) 
#     }
# end
# 
# sieve=sieve_up(999_999)
# 
# n=0
# i=11
# until i>=999_999 
#   n+=i if i%5!=0 and trunc_prime?(i,sieve)
#   puts i if trunc_prime?(i,sieve)
#   i+=2
# end
# puts n
# 
# ##### Problem 38 #####
# def conc_prod(n)
#   l=[]
#   i=1
#   until l.size>9
#     l+=(i*n).to_s.split(//)
#     return [i,l.to_s.to_i] if l.sort.to_s=="123456789"
#     i+=1
#   end
#   [0,0]
# end
# 
# max=0
# 1.upto(10_000) do |i|
#   max=[max,conc_prod(i)[1]].max
# end
# puts max
# 
##### Problem 39 #####
# pmax,cmax=0,0
# (2..998).step(2) do |p|
#   c=0
#   1.upto([p-2,500].min) do |b|
#     if (p*p-2*p*b)%(2*p-2*b)==0 and b<= (p*p-2*p*b)/(2*p-2*b)
#  #     puts "p=#{p}   a=#{b}   b=#{(p*p-2*p*b)/(2*p-2*b)}   c=#{p-b-(p*p-2*p*b)/(2*p-2*b)}   count=#{c}"
#       c+=1
#     end
#   end
#   if c>cmax
#     cmax=c
#     pmax=p
#   end
# end
# puts pmax
# # Method with Pythagorean triplets
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
# t,perim,lim=[3,4,5],[12],1000
# perim+=pyth_triples_perim(t,perim,lim)
# l=[].fill(0,0..lim)
# mp,ml=0,0
# perim.each do |p|
#   x=p
#   while x<=lim
#     l[x]+=1
#     mp,ml=x,l[x] if l[p]>ml
#     x+=p
#   end
# end
# puts mp
# 
# ##### Problem 40 #####
# l,i,d,order=0,1,1,0
# until order>=7
#   l+=i.to_s.length
#   order=l.to_s.length
#   if (l+(i+1).to_s.length).to_s.length>order
#     d*=(i+1).to_s.split(//)[10**order-l-1].to_i
#   end
#   i+=1
# end
# puts d