=begin
##### http://projecteuler.net #####

##### Problem 1 #####
def sum_mult(max,step)
  step*(max/step)*(max/step+1)/2
end

n=1000
puts sum_mult(n-1,3) +sum_mult(n-1,5)-sum_mult(n-1,15)

##### Problem 2 #####
max=4e6
f1,f2=1,1
c=f1+f2
s=0

while f2<max
  s+=c
  f1=f2+c
  f2=f1+c
  c=f1+f2
end
puts s

##### Problem 3 #####
n=600851475143
dmax,p,m=1,1,n

while p!=n
  2.upto(m){|x|
    if m%x==0
      then
      m/=x
      p*=x
      dmax=x
      break
    end
  }
end
puts dmax

##### Problem 4 #####
max=0
(110..990).step(11){ |y|
  999.downto(100) { |x|
    if (x*y).to_s == (x*y).to_s.reverse
      then
      max= [x*y,max].max
      break
    end
  }
}
puts max

##### Problem 5 #####
puts 2**4*3**2*5*7*11*13*17*19

##### Problem 6 #####
# brute force
a=(1..100).inject{|sumsq,n| sumsq+n**2}
b=((1..100).inject{|sum,n| sum +n})**2
puts b-a
# A bit better
s=0
1.upto(100){|x|
  (x+1).upto(100) {|y|
    s+=2*x*y
  }
}
puts s
# Even better : formula

##### Problem 7 #####
n=10001
j=2
primes=[2,3]
while primes.length < n
  x=primes.last+j
  primes.each do |xi|
    if x%xi==0 then
      j+=2
      break
    elsif x<=xi**2 then
      primes << x
      j=2
      break
    end
  end
end
puts primes.last

##### Problem 8 #####
n="73167176531330624919225119674426574742355349194934969835203127745063262395783180169848018694788518438586156078911294949545950173"+\
"79583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934"+\
"23380308135336276614282806444486645238749303589072962904915604407723907138105158593079608667017242712188399879790879227492190169972"+\
"08880937766572733300105336788122023542180975125454059475224352584907711670556013604839586446706324415722155397536978179778461740649"+\
"55149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116"+\
"42717147992444292823086346567481391912316282458617866458359124566529476545682848912883142607690042242190226710556263211111093705442"+\
"17506941658960408071984038509624554443629812309878799272442849091888458015616609791913387549920052406368991256071760605886116467109"+\
"40507754100225698315520005593572972571636269561882670428252483600823257530420752963450"
n=n.split(//)
max=0
0.upto(n.length-1){|i|
  p=1
  0.upto(4){|j| p*= n[i+j].to_i}
  max=[max,p].max
  }
puts max

##### Problem 9 #####
s=1000
3.upto(s/3) { |a|
  (a+1).upto((s-a)/2){|b|
    if a**2 + b**2 == (1000-a-b)**2 then
      puts a*b*(1000-a-b)
      break
    end
  }
}
=end

##### Problem 10 #####
lim=2_000_000
lim=100
sqlim=Math.sqrt(lim).to_i
sieve=[true,true,true,true]
sieve.fill(false,4..(lim-1))
1.upto(sqlim){ |x|
  1.upto(sqlim){ |y|
    n=4*x**2+y**2
    sieve[n]=!sieve[n] if n <= lim && (n%12==1 or n%12==5)
    n=3*x**2+y**2
    sieve[n]=!sieve[n] if n <= lim && n%12==7
    n=3*x**2-y**2
    sieve[n]=!sieve[n] if x>y && n <= lim && n%12==11
  }
}
3.upto(sqlim){ |i|
 if sieve[i] then
   k=1
   while k*i**2<lim
     sieve[k*i**2]=false
     k+=1
   end
 end
}
# 3.upto(sqlim){ |i|
#     sieve.fill(false,(i**2..lim).step(i**2)) if sieve[i]
# }
s=0
2.upto(lim-1){ |i| s+= i if sieve[i]}
puts sieve
