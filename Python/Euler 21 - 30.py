import math

def divisors(n):	
	res=[1]
	div,div2,lim=2,n/2,math.sqrt(n)
	while div<lim:
		while n%div!=0: div+=1
		div2=n/div
		if div>div2 : break
		elif div2==div:
			res+=[div]
			break
		else:
			res+=[div,div2]
		div+=1
	return res #sorted(res)
	
def sum_divisors(n):
	if n==1: return 0
	res=1
	div,div2,lim=2,n/2,math.sqrt(n)
	while div<lim:
		while n%div!=0: div+=1
		div2=n/div
		if div>div2 : break
		elif div2==div:
			res+=div
			break
		else:
			res+=div+div2
		div+=1
	return res

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
	
def pick(n,l): # List of ways to pick n elements from the list l (order doesn't matter)
	indx=[0]*n
	L=len(l)
	res=[]
	while indx[-1]<L:
		res.append([ l[indx[i]] for i in range(n)])
		indx[0]+=1
		if indx[0]==L:
			for i in range(n-1):
				if indx[i]==L:
					indx[i+1]+=1
					indx[i]=0
			for i in range(n-2,-1,-1):
					indx[i]=max(indx[i],indx[i+1])
	return res

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

# # Problem 21
# tot=0
# lim=10000
# sieve=[True]*lim
# for i in range(2,lim):
# 	if sieve[i]: 
# 		s=sum_divisors(i)
# 		t=sum_divisors(s)
# 		if s!=i and t==i: 
# 			print i,s
# 			tot+=i+s
# 		if s<lim: sieve[s]=False
# 		if t<lim: sieve[t]=False
# print tot

# # Problem 22
# import re
# file=open("names.txt","r")
# list=sorted(filter(None,re.split('\W',file.read())))
# s=0
# i=1
# for words in list:
# 	s+=i*sum([ ord(letter)-ord('A')+1 for letter in words])
# 	i+=1
# print s

##  Problem 23
# def is_sum_of_2(n,list):
# 	for i1 in range(len(list)):
# 		if list[i1]>n: break
# 		for i2 in range(i1,len(list)):
# 			if list[i1]+list[i2]>n: break
# 			if list[i1]+list[i2]==n: return True
# 	return False
# 
# lim=28123
# sieve=[False]*lim
# abundant=[i for i in range(12,lim) if sum_divisors(i)>i ]
# # for i in range(24,lim):
# # 	if not is_sum_of_2(i,abundant): # super slow 
# # 		tot+=i
# # print tot
# 
# for el1 in abundant:
# 	for el2 in abundant:
# 		if el1+el2>=lim: break
# 		sieve[el1+el2]=True
# print sum([i for i in range(1,lim) if not sieve[i] ])
# 
# # abundants = set(i for i in range(1,28124) if sum_divisors(i)> i)
# # def abundantsum(i):
# # 	return any(i-a in abundants for a in abundants)
# # print sum(i for i in range(1,28124) if not abundantsum(i))

# # Problem 24
# p=permutations([0,1,2,3,4,5,6,7,8,9])
# x=sorted([int(''.join(map(str,el))) for el in p])
# print x[999999]

# # Problem 25
# fold,fnew=1,1
# i=2
# while len(str(fnew))<1000:
# 	fold,fnew=fnew,fnew+fold
# 	i+=1
# print fnew, i, len(str(fnew))

# # Problem 26
# dmax,cmax=0,0
# for d in range(2,1001):
# 	rems=[]
# 	rem=1
# 	while rem!=0 and not rem in rems:
# 		rems.append(rem)
# 		rem=(10*rem)%d
# 	if rem!=0 and len(rems)-rems.index(rem)>cmax:
# 		cmax,dmax=len(rems)-rems.index(rem),d
# print dmax

# # Problem 27
# def is_in_ordered(x,l):
# 	imin,imax=0,len(l)-1
# 	while imax-imin>1:
# 		imid=(imax+imin)/2
# 		if x==l[imid]:
# 			return True
# 		elif x>l[imid]:
# 			imin=imid
# 		else:
# 			imax=imid
# 	return x==l[imin] or x==l[imax]
# 	
# def length_prime(a,b,primes):
# 	n=0
# #	while n**2+n*a+b in primes:
# 	while is_in_ordered(n**2+n*a+b,primes):
# 		n+=1
# 	return n
# 
# def possible_a(b,primes,lim):
# 	a=[]
# 	i=0
# 	ac=0
# 	while abs(ac)<lim:
# 		ac=b-primes[i]-1
# 		a+=[ac]
# 		i+=1
# 	return a
# 
# lim=1000
# primes=primes_until(lim*lim)
# b,i=0,0
# amax,bmax,cmax=0,0,0
# while b<lim:
# 	b=primes[i]
# 	i+=1
# 	for a in possible_a(b,primes,lim):
# 		l=length_prime(a,b,primes)
# 		if l>cmax:
# 			amax,bmax,cmax=a,b,l	
# print amax,bmax,cmax,amax*bmax # 61 971 71 -59231


# # Problem 28
# a=1001
# print sum([4*n*n+10*(n+1) for n in range(1,a,2) ])

# # Problem 29
# lim=100
# count={}
# for a in range(2,lim+1):
# 	for b in range(2,lim+1):
# 		count[a**b]=0
# print len(count)

# Problem 30
exp=5
s=0
for dig in range(2,7):
	for comb in pick(dig,range(10)):
		tmp=sum([d**exp for d in comb])
		if sorted(str(tmp))==sorted(map(str,comb)):
			print comb, tmp
			s+=tmp
print s
