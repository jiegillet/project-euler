# ##### http://projecteuler.net #####
# 
# ##### Problem 21 #####
# def sum_div(n)
#   s=1
#   sq=Math.sqrt(n).to_i
#   2.upto(sq){ |i|  
#     s+=i+n/i if n%i==0
#   }
#   s-=sq if sq**2==n
#   s
# end
# 
# s=0
# 3.upto(10_000){ |i|
#     x=sum_div(i)
#     s+= i + x if i==sum_div(x) && x>i
#   }
# puts s
# 
# ##### Problem 22 #####
# def quicksort(l)
#    return [] if l==[]
#     p,*xp=l
#     quicksort(xp.select{ |x| x<p }) +[p]+ quicksort(xp.select{ |x| x>=p  })
# end
# 
# f=File.open("names.txt","r").readline.split(/\",\"|\"/)
# f=quicksort(f)
# 
# t=0
# for i in 0...f.length
#   s=0
#   f[i].each_byte do |c|
#     s+=c-64
#   end
#   t+= s*i
# end
# puts t
# 
# ##### Problem 23 #####
# def sum_div(n)
#   s=1
#   2.upto(Math.sqrt(n).to_i){ |i|
#     p=1
#     while n%i==0
#       p=p*i+1
#       n/=i
#     end
#     s*=p
#   }
#   s*=(1+n) if n>1
#   s
# end
# 
# lim=28123
# 
# n=[]
# ab=[]
# 12.upto(lim){ |i|
#   if 2*i<sum_div(i) then
#     ab << i  
#     ab.each{ |x|
#       break if i+x> lim
#       n[x+i]=x+i
#     }
#   end
#   }
# 
# s=lim*(lim+1)/2
# n.each{ |i| s-=i if i!=nil}
# puts s
#
# ##### Problem 24 #####
# def fact(n)
#   (1..n).inject(1){ |p,i| p*i}
# end
# 
# N=10
# n=1_000_000
# dig=[]
# 0.upto(N-1){ |i| dig+=[i] }
# 
# res=[]
# n-=1
# (N-1).downto(0){ |i|
#   d=n/fact(i)
#   res+=[dig[d]]
#   dig-=[dig[d]]
#   n-=d*fact(i)
# }
# print res
# 
# ##### Problem 25 #####
# f1,f2=1,1
# c=2
# until f2>=10**999
#   f2+=f1
#   f1=f2-f1
#   c+=1
# end
# puts c
# 
# ##### Problem 26 #####
# max=0
# dmax=0
# 1.upto(999){ |den|
#   num=1
#   c=0
#   n=[]
#   until n[num%den]!=nil
#     n[num%den]=c
#     num=10*(num%den)
#     c+=1
#   end
#   if max<c-n[num%den]
#     max=c-n[num%den]
#     dmax=den
#   end
# 
#   n.clear
# }
# puts "#{dmax}    #{max}"
# 
# ##### Problem 27 #####
# require 'mathn'
# 
# class Integer
#   def is_prime?
#     n=self
#     return true if n==2
#     return false if n%2==0
#     (3..(s=Math.sqrt(n).to_i)).step(2){ |i|
#       return false if n%i==0
#     }
#     return false if s**2 == self
#     true
#   end
# end
# 
# nmax,c1,c2=0,0,0
# prime_b = Prime.new
# 
# until (b=prime_b.next)>1000
#   -999.upto(999){ |a|
#     n=0
#     while (n**2+a*n+b)>0 and (n**2+a*n+b).is_prime?
#       n+=1
#     end
#     if n>nmax
#       nmax=n
#       c1,c2=a,b
#     end
#   }
# end
# puts "#{c1}   #{c2}   #{nmax}  #{c1*c2}"
# 
# ##### Problem 28 #####
# sum,num,n=1,1,2
# until n-1==1001
#   4.times{ sum+=num+=n }
#   n+=2
# end
# puts sum
# 
# ##### Problem 29 #####
# l={}
# 2.upto(100){ |a|
#   2.upto(100){ |b|  
#     l[a**b]="Wepeee =D"
#   }  
# }
# puts l.length
# 
# ##### Problem 30 #####
# s,n=0,0
# 2.upto(400_000){ |i| 
#   if i==i.to_s.split(//).inject(0){ |p,a|  p+(a.to_i)**5 } then
#     s+= i
#     puts i
#   end
# }
# puts "Sum: #{s}"