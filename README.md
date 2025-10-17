# Meteoroloji-Machine-Learning
Meteorolojide makine öğrenmesi, MYZ312 Kapsamında AHMET OZTOPAL tarafından yazılmıştır. fortran kodu olduğu için githubda bulunmamaktadır :)


! Bu program doğrusal regresyon analizi yapar.
!                 Y = aX + b
! Bu fortran kodu Meteorolojide Makina Öğrenmesi - MYZ 312 dersi kapsamında,
! dersin öğretim üyesi Ahmet Öztopal tarafından yazılmıştır.
! Kaynak göstermek şartıyla kullanılabilir.
! Eposta: oztopal@itu.edu.tr

program reganaliz
implicit none

! Kod içinde kullanılan değişken ve sabitlerin tanımlanması
real y,x,xy,x2,yort,xort,xyort,x2ort,topx,topy
real topx2,topxy,a,b,hata,ytah,yhata
integer i,say,secim,verisay
character dosya*8,dosya2*8


write(*,*)
write(*,*)

print*, '*************************************************************************'
print*, 'Bu program doğrusal regresyon analizi yapar.'
print*, '                Y = aX + b'
print*, 'Bu fortran kodu Meteorolojide Makina Öğrenmesi - MYZ 312 dersi kapsamında,'
print*, 'dersin öğretim üyesi Ahmet Öztopal tarafından yazılmıştır.'
print*, 'Kaynak göstermek şartıyla kullanılabilir.'
print*, 'Eposta: oztopal@itu.edu.tr'
print*, '*************************************************************************'

write(*,*)
write(*,*)

! Veri dosyasının açılması
print*, 'Veri dosyası 2 sütunluk olmalı ve Y,X sırasında olmalıdır.'
print*, 'Veri dosyasının adı nedir? (Dosya ismi xxxx.dat şeklinde toplam 8 karakterden oluşmalı.)'
read*, dosya

print*, 'Veri sayısı (dosyadaki satır sayısı) nedir?'
read*, verisay


open(11,file=dosya,status='old')
open(12,file='sonuclar.dat',status='unknown')

! Toplamalar için kullanılacak değişkenlere ilk değer atamaları
topy=0
topx=0
topxy=0
topx2=0
say=0

write(*,*)
write(*,*) '********************'
write(*,25) '    Y         X'

write(12,*)
write(12,*) '********************'
write(12,25) '    Y         X'
25 format(a15)

! Verilerin okunması, toplamaların yapılması ve verilerin ekrana yazılması
do i=1,verisay
read(11,*) y,x
write(*,26) y,x
write(12,26) y,x
26 format(2(1x,f7.2))
xy=x*y
x2=x*x
topy=topy+y
topx=topx+x
topxy=topxy+xy
topx2=topx2+x2
say=say+1
enddo

! Ortalamaların hesaplanması
yort=topy/say
xort=topx/say
xyort=topxy/say
x2ort=topx2/say

write(*,*)
write(12,*)
close(11)

! Hesaplanan ortalamaların ekrana yazılması
write(*,*) '********************'
print*, 'HESAPLANAN ORTALAMALAR'
print 27, 'Y ortalama= ',yort
print 27, 'X ortalama= ',xort
print 27, 'XY ortalama= ',xyort
print 27, 'X2 ortalama= ',x2ort

write(12,*) '********************'
write(12,*) 'HESAPLANAN ORTALAMALAR'
write(12,27) 'Y ortalama= ',yort
write(12,27) 'X ortalama= ',xort
write(12,27) 'XY ortalama= ',xyort
write(12,27) 'X2 ortalama= ',x2ort
27 format(a14,f7.2)

write(*,*)

! a ve b katsayılarının hesaplanması ve ekrana yazılması
a=(xyort-(xort*yort))/(x2ort-xort**2)
b=yort-(a*xort)

write(*,*) '********************'
print 28, 'Denklem tipi: Y = a X + b'
28 format(a25)
print 29, 'a katsayısı= ',a
print 29, 'b katsayısı= ',b
29 format(a14,f8.4)
print*, 'Veri setinden eğitim aşaması tamamlanarak ilişki denklemindeki a ve b katsayıları öğrenilmiştir.'

write(12,*) '********************'
write(12,28) 'Denklem tipi: Y = a X + b'
write(12,29) 'a katsayısı= ',a
write(12,29) 'b katsayısı= ',b
write(12,*) 'Veri setinden eğitim aşaması tamamlanarak ilişki denklemindeki a ve b katsayıları öğrenilmiştir.'

write(*,*) 
write(*,*) '********************'

write(12,*)
write(12,*) '********************'


! Model hatası hesabı (hata = y - ytah)
open(11,file=dosya,status='old')

print*, '        MODEL HATALARI'
write(12,*) '        MODEL HATALARI'

do i=1,say
read(11,*) y,x
ytah=a*x+b
hata=y-ytah
yhata=(hata/y)*100
print 30, 'y',i,'=',y,'    ytah',i,'=',ytah,'   hata',i,'=',hata,'    yüzde hata',i,'=',yhata
write(12,30) 'y',i,'=',y,'    ytah',i,'=',ytah,'   hata',i,'=',hata,'    yüzde hata',i,'=',yhata
30 format(a1,i2,a1,f7.2,a8,i2,a1,f7.2,a8,i2,a1,f7.3,a14,i2,a1,f6.2)
enddo

write(*,*)
write(*,*) '********************'

write(12,*)
write(12,*) '********************'

! Tahmin aşaması
write(*,*) 'Tahmin için dosya mı yüklemek istersiniz yoksa veri girişini klavyeden mi yapmak istersiniz?'
write(*,*) 'Dosya için 1 ve klavye içinse 2ye basınız.'
50 read*, secim

if(secim.eq.1) then
    print*, 'Dosya adı giriniz. Dosya adı xxxx.dat şeklinde 8 karakterlik olmalıdır.'
    read*, dosya2
    write(*,*) 'Veri sayısı nedir?'
    read*, verisay
    open(13,file=dosya2,status='old')
    write(*,*)
    write(*,*) '********************'
    print*, '       TAHMİNLER'
    write(12,*) '       TAHMİNLER'

    do i=1,verisay
    read(13,*) x
    y=a*x+b
    print 31, 'X',i,'= ',x,'    Y',i,'= ',y
    write(12,31) 'X',i,'= ',x,'    Y',i,'= ',y
31 format(a1,i2,a2,f7.2,a5,i2,a2,f7.2)
    enddo
elseif(secim.eq.2) then
    print*, 'Veri sayısı nedir?'
    read*, verisay    
    write(*,*)
    write(*,*) '********************'
    print*, '       TAHMİNLER'
    write(12,*) '       TAHMİNLER'    
    do i=1,verisay
    print*, 'x verisini giriniz.'
    read*, x
    y=a*x+b
    print 31, 'X',i,'= ',x,'    Y',i,'= ',y
    write(12,31) 'X',i,'= ',x,'    Y',i,'= ',y
    enddo
else
    print*, 'Yanlış giriş! Dosya için 1 ve klavye içinse 2ye basınız.'
    goto 50        
endif
end program reganaliz

