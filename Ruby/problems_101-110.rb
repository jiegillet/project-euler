# ! /usr/local/bin/ruby19
##### http://projecteuler.net #####

# #### Problem 102 #####
# # http://en.wikipedia.org/wiki/Barycentric_coordinates_%28mathematics%29
# m=[]
# File.open("triangles.txt","r").each_line do |line|
#   m<<line.chomp.split(/,/).map{ |x| x.to_i  }
# end
# c=0
# m.each do |e|
#   l1=(-(e[3]-e[5])*e[4] + e[5]*(e[2]-e[4]))/((e[0]-e[4])*(e[3]-e[5]) - (e[1]-e[5])*(e[2]-e[4].to_f))
#   l2=((e[1]-e[5])*e[4] - e[5]*(e[0]-e[4]))/((e[0]-e[4])*(e[3]-e[5]) - (e[1]-e[5])*(e[2]-e[4].to_f))
#   c+=1 if 0<l1 and 0<l2 and l1+l2<1
# end
# puts c

# #### Problem 112 #####
# p,i=0,100
# until 100*p==99*i do
#   i+=1
#   p+=1 if i.to_s.split(//).sort.to_s.to_i != i and i.to_s.split(//).sort{ |x,y| y<=>x }.to_s.to_i!=i
# end
#   puts i


















