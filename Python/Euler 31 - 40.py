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

# # Problem 31
# s=200
# a=0
# tot=0
# for p200 in range(s/200+1):
# 	a=200*p200
# 	for p100 in range((s-a)/100+1):
# 		a=200*p200+p100*100
# 		for p50 in range((s-a)/50+1):
# 			a=200*p200+p100*100+p50*50
# 			for p20 in range((s-a)/20+1):
# 				a=200*p200+p100*100+p50*50+p20*20
# 				for p10 in range((s-a)/10+1):
# 					a=200*p200+p100*100+p50*50+p20*20+p10*10
# 					for p5 in range((s-a)/5+1):
# 						a=200*p200+p100*100+p50*50+p20*20+p10*10+p5*5
# 						tot+=(s-a)/2+1
# print tot

# # Problem 32
# perm=permutations(range(1,10))
# l={}
# for p in perm :
# 	if int(''.join(map(str,p[:2])))*int(''.join(map(str,p[2:5])))==int(''.join(map(str,p[5:]))):
# 		print ''.join(map(str,p[:2])),''.join(map(str,p[2:5])),''.join(map(str,p[5:]))
# 		l[int(''.join(map(str,p[5:])))]=0
# 	if p[0]*int(''.join(map(str,p[1:5])))==int(''.join(map(str,p[5:]))):
# 		print p[0],''.join(map(str,p[1:5])),''.join(map(str,p[5:]))
# 		l[int(''.join(map(str,p[5:])))]=0
# print sum([k for k in l])

# # Problem 33
# for num in range(1,10):
# 	for den in range(num+1,10):
# 		for dig in range(1,10):
# 			if den*(10*num+dig)==num*(10*dig+den) or den*(10*dig+num)==num*(10*den+dig) or den*(10*num+dig)==num*(10*den+dig) or den*(10*dig+num)==num*(10*dig+den):
# 				print num,den, dig 
# # final answer, pen and paper: 100
		
# # Problem 34
# fact=[math.factorial(i) for i in range(10)]
# s=0
# for i in range(10,1000000):
# 	if sum([fact[int(k)] for k in str(i)])==i:
# 		s+=i
# 		print i
# print s

# # Problem 35
# def rot(n):
# 	dig=str(n)
# 	return list(set([ int(''.join(dig[i:]+dig[:i]))  for i in range(len(dig))]))
# 
# lim=1000000
# primes=primes_sieve(lim)
# c=0
# for p in range(lim):
# 	if not primes[p]:
# 		pass
# 	else:
# 		r=rot(p)
# 		if all( primes[d] for d in r):
# 			c+=len(r)
# 			for d in r:
# 				primes[d]=False
# print c	


# # Problem 36
# c=0
# for i in range(1,1000000,2):
# 	if str(i)==str(i)[::-1] and "{0:b}".format(i)=="{0:b}".format(i)[::-1]:
# 		c+=i
# 		print i,"{0:b}".format(i)
# print c


# # Problem 37
# def is_truncable(n,primes):
# 	if n<10:
# 		return False
# 	n2=str(n)
# 	return all([is_in_ordered(int(''.join(n2[i:])),primes)  for i in range(len(n2))]+[is_in_ordered(int(''.join(n2[:i])),primes)  for i in range(1,len(n2))])
# 
# primes=primes_until(1000000)
# s=0
# for p in primes:
# 	if is_truncable(p,primes):
# 		s+=p
# 		print p
# print "sum is %d"%(s)

# # Problem 38
# def are_pandigital(l): # Is a list of number l pandigital?
# 	return ''.join(sorted(''.join(map(str,l))))=='123456789'
# 
# pan=[]
# for i in range(1,10000):
# 	p=[i]
# 	c=1
# 	while sum([len(str(k)) for k in p])<9:
# 		c+=1
# 		p+=[c*i]
# 	if sum([len(str(k)) for k in p])==9 and are_pandigital(p):
# 		pan.append(int(''.join(map(str,p))))
# print pan
# print max(pan)

# # Problem 39
# smax,pmax=0,0
# for p in range(2,1000,2):
# 	sol=0
# 	for c in range(3,p/2):
# 		for a in range(1,c-2):
# 			if a**2+(p-c-a)**2==c**2:
# 				sol+=1
# 	if sol>smax:
# 		smax,pmax=sol,p
# print pmax				

# Problem 40
n='0'
i=0
while len(n)<1E6+1:
	i+=1
	n+=str(i)
print int(n[1])*int(n[10])*int(n[100])*int(n[1000])*int(n[10000])*int(n[100000])*int(n[1000000])