import math

def permutations(list):
	if len(list)==1:
		return [list]
	p=permutations(list[1:])
	L=len(p)
	p*=len(list)
	for i in range(len(list)):
		for j in range(L):
			p[i*L+j]=p[i*L+j][0:i]+[list[0]]+p[i*L+j][i:]
	return p
	
def primes_until(k):
	primes=[2,3]
	n=3
	while n+2<k:
		n+=2
		p=True
		for p in primes:
			if p*p>n: break
			if n%p==0: p=False; break
		if p: primes.append(n)
	return primes

def is_in_ordered(x,l):
	imin,imax=0,len(l)-1
	while imax-imin>1:
		imid=(imax+imin)/2
		if x==l[imid]:
			return True
		elif x>l[imid]:
			imin=imid
		else:
			imax=imid
	return x==l[imin] or x==l[imax]

def primes_sieve(k):
	primes=[True]*k
	primes[0],primes[1]=False,False
	for i in range(4,k,2):
		primes[i]=False
	n=3
	while n<k:
		if primes[n]:
			for i in range(2*n,k,n):
				primes[i]=False
		else:
			pass
		n+=2
	return primes

def is_prime(k):
	if k in [0,1]:
		return False
	if k in [2,3]:
		return True
	if k%2==0:
		return False
	n=1
	while n*n<=k:
		n+=2
		if k%n==0:
			return False
	return True

# # Problem 41
# def is_n_pandigital(n):
# 	return sorted(str(n))==[str(i) for i in range(1,len(str(n))+1)]
# 
# num=permutations([i for i in range(1,8)])
# nmax=0
# for n in num:
# 	p=int(''.join(map(str,n)))
# 	if is_prime(p) and p>nmax:
# 		nmax=p
# print nmax
# 
# # print max([int(''.join(map(str,n))) for n in num if is_prime(int(''.join(map(str,n))))])

# # Problem 42
# def is_triangle(t):
# 	return math.sqrt(1+8*t)%2==1
# 
# file=open("text/words.txt","r")	
# #txt=file.read().split('","') # This is OK only if you get rid of the first and last " in the file
# txt=eval( '[' + file.read() + ']' )
# c=0
# for word in txt:
# 	if is_triangle(sum([ord(let)-64 for let in word])):
# 		c+=1
# print c

# Problem 43
# # num=permutations([i for i in range(10)])
# # primes=[2,3,5,7,11,13,17]
# # print sum([ int(''.join(map(str,n)))  for n in num if all([  int(''.join(map(str,n[i:i+3])))%primes[i-1]==0 for i in range(1,8)]) ])
# # # solution: 16695334890
# 
# primes=[2,3,5,7,11,13,17]
# num1=permutations([i for i in range(1,10)])
# num2=permutations([i for i in range(10) if i!=5])
# for i in range(len(num1)):
# 	num1[i].insert(5,0)
# 	num2[i].insert(5,5)
# print sum([ int(''.join(map(str,n)))  for n in num1+num2 if all([  int(''.join(map(str,n[i:i+3])))%primes[i-1]==0 for i in range(1,8)]) ])

# # Problem 44
# def penta(n):
# 	return n*(3*n-1)/2
# def is_penta(p):
# 	return (math.sqrt(1+24*p)+1)%6==0
# lim=10000
# flag=False
# for j in range(1,lim):
# 	for k in range(j,lim):
# 		if is_penta(penta(j)+penta(k)) and is_penta(penta(k)-penta(j)):
# 			print j,k,penta(j)+penta(k),penta(k)-penta(j)
# 			flag=True
# 			break
# 	if flag:
# 		break
		
# # Problem 45
# def is_triangle(t):
# 	return math.sqrt(1+8*t)%2==1
# def is_penta(p):
# 	return (math.sqrt(1+24*p)+1)%6==0
# def is_hexa(h):
# 	return (math.sqrt(1+8*h)+1)%4==0
# def gen_triangle(init):
# 	n=init
# 	while True:
# 		yield n*(n+1)/2
# 		n+=1
# def gen_penta(init):
# 	n=init
# 	while True:
# 		yield n*(3*n-1)/2
# 		n+=1
# def gen_hexa(init):
# 	n=2
# 	while True:
# 		yield n*(2*n-1)
# 		n+=1
# 	
# # i=40755+1
# # i=2
# # found=False
# # while not found:
# # 	while not(is_hexa(i) and is_penta(i)):
# # 		i+=1
# # 	if is_triangle(i):
# # 		found=True
# # print i # super duper slow # and stupid because you should just test consecutive hex numbers
# 
# a,b,c=gen_triangle(285+1),gen_penta(165+1),gen_hexa(143+1) # super duper fast
# a_val,b_val,c_val=0,0,0
# found=False
# while not found:
# 	c_val=c.next()
# 	while b_val<c_val:
# 		b_val=b.next()
# #	while a_val<c_val:
# #		a_val=a.next()
# 	found=b_val==c_val #and a_val==b_val # Every hexagonal number is also a triangle number
# print c_val

# # Problem 46
# lim=10000
# primes=primes_until(lim)
# found=False
# n=7
# while not found:
# 	n+=2
# 	if is_in_ordered(n,primes):
# 		pass
# 	for p in primes:
# 		if p>n:
# 			found=True
# 			break
# 		if math.sqrt((n-p)/2)%1==0:
# 			break
# print n

# # Problem 47
# def factors(n,primes):
# 	f={}
# 	while True:
# 		for p in primes:
# 			while n%p==0:
# 				f[p]=f.get(p,0)+1
# 				n/=p
# 			if n==1:
# 				return f
# 
# def num_factors(n,primes):
# 	c=0
# 	while True:
# 		for p in primes:
# 			if n%p==0:
# 				c+=1
# 			while n%p==0:
# 				n/=p
# 			if n==1:
# 				return c
# d=4
# primes=primes_until(500000)
# found=False
# n=4
# f=[[0]]*d
# while not found:
# 	n+=1
# 	f[n%d]=num_factors(n,primes)
# 	found=f==[d]*d
# print n-d+1

# # Problem 48
# lim=1000
# print sum([i**i for i in range(1,lim+1)])%10**10

# # Problem 49
# primes0=primes_until(10000)
# primes=[p for p in primes0 if p>999]
# for p1 in primes:
# 	for p2 in primes:
# 		if p2>p1:
# 			if(is_in_ordered(2*p2-p1,primes)):
# 				if sorted(str(p1))==sorted(str(p2)) and sorted(str(p1))==sorted(str(2*p2-p1)):
# 					print p1,p2,2*p2-p1

# Problem 50
lim=1000000
primes=primes_until(lim)
l=0
s=0
for p in primes:
	if s>lim:
		break
	s+=p
	l+=1
l0=l+1
found=False
while not found:
	l-=1
	for k in range(l0-l):
		if is_in_ordered(sum(primes[k:k+l+1]),primes):
			print l,sum(primes[k:k+l+1])
			found=True
			break


	
	
	
	