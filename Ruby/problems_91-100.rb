# ! /usr/local/bin/ruby19
##### http://projecteuler.net #####

#### Problem 92 #####
# # Brute force
# c=0
# l=[]
# l[89]=true
# l[1]=false
# 1.upto(999_999) do |n|
#   k=[]
#   until l[n]!=nil  do
#     n=n.to_s.split(//).inject(0){ |s,e| s+(e.to_i)**2 }
#     k<<n
#   end
#   c+=1 if l[n]
#   k.each{ |i| l[i]=l[n] } if k.length>1 
# end
# puts c

# def fact(n)
#   return 1 if n<2
#   (2..n).inject(1){ |p,i| p*i }
# end
# 
# n,k=10,7
# l=[]
# l[1],l[89]=false,true
# c=0
# tab=[k].fill(0,1..n-1)
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
#   x=(0..n-1).map{ |e| e.to_s*tab[e] }.to_s.to_i
#   m=[x]
#     until l[x]!=nil  do
#       x=x.to_s.split(//).inject(0){ |s,e| s+(e.to_i)**2 }
#       m<<x
#     end
#     c=c+(0..n-1).inject(fact(k)){ |d,e| d/fact(tab[e]) } if l[x]
#     m.each{ |e| l[e]=l[x] } if m.length>1
# end
# puts c


#### Problem 99 #####
m=[]
File.open("base_exp.txt","r").each_line do |line|
  m<<line.chomp.split(/,/).map{ |x| x.to_i  }
end
x=m
(m.length-1).times do
   # m[0][0]**(m[0][1]/m[1][1].to_f)>m[1][0] ?  m-=[m[1]] : m-=[m[0]]
   Math.log10(m[0][0].to_f)*m[0][1]>Math.log10(m[1][0].to_f)*m[1][1] ?  m-=[m[1]] : m-=[m[0]]

end
p x.index(m[0])+1




















