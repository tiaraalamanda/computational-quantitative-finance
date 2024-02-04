clc;
clear;
%% Baca data
file = xlsread('C:\_PELAJARAN\Komputasi Matematika D\uas.xlsx') %Membaca file uas.xlsx, direktorinya disesuaikan
p = file(:,1) %periode atau jangka waktu
i = file(:,2) %tingkat suku bunga tahunan
vlookup = @(file,p,tahun,i) file((file(:,p)==tahun),i);
%@(...) merupakan pendefinisian variabel untuk fungsi 
%file( (file(;,p)==tahun , i )
%file(;,p)==tahun adalah pencarian data tahun yang kita input, pada tabel
%lalu memunculkan i yang bersesuaian dengan tahun tersebut.
%% Plan A atau B

plan = input('Masukkan planmu (A atau B): ','s'); %input plan A atau B

if plan == 'A' %penggunaan if untuk kondisi plan A
    target = input('Masukkan target saldo: ');
    tahun = input('Pilih jangka waktu menabung (0-5 tahun untuk kelipatan 0.5 tahun): ');
    while tahun ~= p
        %penggunaan while berguna agar user menginput tahun sesuai
        %ketentuan pada tabel. Jika tidak sesuai, maka user diminta untuk
        %menginput tahun sampai benar.
        disp('Jumlah tahun tidak sesuai ketentuan');
        tahun = input('Pilih jangka waktu menabung (0-5 tahun untuk kelipatan 0.5 tahun): ');
    end
    verif = input('Apakah anda mau menambah Down Payment? minimal 10% (Y atau N): ','s');
    %verif adalah verifikasi apakah user ingin menambah DP
    if verif == 'Y'
        DP = input('Masukkan jumlah DP minimal 10% dari target saldo :');
        while DP < 0.1*target 
            %while berguna agar user memasukkan dp yang lebih dari atau
            %sama dengan 10%, jika tidak maka user dipaksa untuk menginput
            %kembali DP.
            disp('Sistem DP tidak dapat dijalankan pilih ulang besar DP');
            DP = input('Masukkan jumlah DP minimal 10% dari target saldo: ');
        end
        i=vlookup(file,1,tahun,2)+0.005; %mencari data berdasarkan tahun yang diinput
        fprintf('Tingkat Suku Bunga Tahunan adalah %f persen \n',i*100); %print tingkat suku bunga
        j=(1+i)^(tahun/12)-1; %ubah tingkat suku bunga tahunan menjadi bulanan
        Sn = ((1+j)^(tahun*12)-1)/(1-(1/(1+j))); %mencari akumulasi anuitas awal
        angsuran=(target-DP)/Sn; %besar angsuran
        fprintf('Besar angsuran per bulan untuk memperoleh %d dalam %f tahun dengan DP %d adalah %f\n',target,tahun,DP,angsuran);
    elseif verif == 'N'
        i=vlookup(file,1,tahun,2); %mencari data berdasarkan tahun yang diinput
        fprintf('Tingkat Suku Bunga Tahunan adalah %f persen \n',i*100); %print tingkat suku bunga
        j=(1+i)^(tahun/12)-1; %ubah tingkat suku bunga tahunan menjadi bulanan
        Sn = ((1+j)^(tahun*12)-1)/(1-(1/(1+j))); %mencari akumulasi anuitas awal
        angsuran=target/Sn; %besar angsuran
        fprintf('Besar angsuran per bulan untuk memperoleh %d dalam %f tahun adalah %f\n',target,tahun,angsuran);
        %print kesimpulan
    end
%%Plan B
elseif plan == 'B' %penggunaan elseif, untuk kondisi plan B
    tabung = input('Berapa saldo yang ingin anda tabung per bulan: ');
    tahun = input('Jangka waktu menabung (0-5 tahun untuk kelipatan 0.5 tahun): ');
    while tahun ~= p 
        %penggunaan while agar user menginput tahun sesuai ketentuan
        disp('Jumlah tahun tidak sesuai ketentuan');
        tahun = input('Pilih jangka waktu menabung (0-5 tahun untuk kelipatan 0.5 tahun): ');
    end
    i=vlookup(file,1,tahun,2); %mencari nilai i dalam tabel dengan fugnsi vlookup
    fprintf('Tingkat Suku Bunga Tahunan %f persen \n',i*100) %print tingkat suku bunga
    j=(1+i)^(tahun/12)-1; %ubah tingkat suku bunga tahunan ke bulanan
    Sn = ((1+j)^(tahun*12)-1)/(1-(1/(1+j))); %mencari akumulasi anuitas awal
    target=Sn*tabung; %mencari besar tabungan yang diperoleh
    fprintf('Besar tabungan yang diperoleh dengan menabung %d per bulan dalam %f tahun adalah %f\n',tabung,tahun,target)
else
    fprintf('Erorr') %jika selain plan A atau B yang dipilih, maka muncul output Error
end

        
        