#!/usr/bin/env python
# encoding: utf-8
"""
untitled.py

Created by Jérémie on 2013-10-26.
Copyright (c) 2013 __MyCompanyName__. All rights reserved.
"""

"""
# Problem 1
lim=1000
s=0
for i in range(lim):
	if i%3==0 or i%5==0:
		s+=i
print s

print sum([x for x in range(1000) if x % 3== 0 or x % 5== 0])
"""
"""
# Problem 2
lim=4000000
f1,f2,s=1,1,0
while f2<lim:
	f1,f2=f2,f1+f2
	if f2%2==0: s+=f2
print s
"""
""""
# Problem 3
num=600851475143
while num>1:
	div=2
	while num%div!=0 and div!=num:
		div+=1
	num/=div
print div
"""
"""
# Problem 4
max=0
for i in range(999,99,-1):
	for j in range(999,i-99,-1):
		if str(i*j)==str(i*j)[::-1] and i>max:
			max=i*j
print max
"""			
"""
# Problem 5
print 2**4*3**2*5*7*11*13*17*19
"""	
"""
# Problem 6
print sum(range(1,101))**2- sum([e**2 for e in range(1,101)])
"""
"""
# Problem 7
primes=[2,3]
n=3
# while len(primes)<10001:
# 	n+=2
# 	if not 0 in [n%p for p in primes]:
# 		primes.append(n)
# print primes[-1] # 45 seconds
while len(primes)<100001:
	n+=2
	p=True
	for p in primes:
		if p*p>n: break
		if n%p==0: p=False; break
	if p: primes.append(n)
print primes[-1] # .3 seconds for 10001 # 6 second for 100001
"""
"""
# Problem 8
num=str(7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450)
print max( int(num[i])*int(num[i+1])*int(num[i+2])*int(num[i+3])*int(num[i+4])  for i in range(len(num)-4))
"""
"""
# Problem 9
sol=0
for i in range(1000,2,-1):
	for j in range(i-1,2,-1):
		if i**2==j**2+(1000-i-j)**2:
			sol=i*j*(1000-i-j)
			break
	if sol>0: break
print sol
"""


#Problem 10
primes=[2,3]
n=3
while primes[-1]<2E6:
	n+=2
	p=True
	for p in primes:
		if p*p>n: break
		if n%p==0: p=False; break
	if p: primes.append(n)
print sum(primes)-primes[-1]
