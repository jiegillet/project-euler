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

# # Problem 51
# primes0=primes_until(1E6)
# primes=[p for p in primes0 if p>9999]
# for p in primes:
# 	dig=''
# 	for let in str(p/10):
# 		if str(p/10).count(let)==3:
# 			dig=let
# 			break
# 	if dig!='':
# 		c=0
# 		for d in '0123456789':
# 			if(is_in_ordered(int(str(p/10).replace(dig,d)+str(p%10)),primes)):
# 				c+=1
# 		if c>7:
# 			print c,p
# 			break

# Problem 52


# Problem 53
# Problem 54
# Problem 55
# Problem 56
# Problem 57
# Problem 58
# Problem 59
# Problem 60

	
	
	
	