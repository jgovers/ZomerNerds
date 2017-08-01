%% Cleaning
close all
clearvars
clc

%% Loading
TimeStamp = '2017_08_01_0841';
addpath('C:\Users\jagov\Dropbox\ZomerNerds\DeBug\'TimeStamp)
tdfread('Test18.SrvD.dbg');
tdfread('Test18.SrvD.dbg2');

%% Formatting
TimeU       = Time(1,:);
Time        = str2num(Time(2:end,:));

GenSpeedU   = GenSpeed(1,:);
GenSpeed    = str2num(GenSpeed(2:end,:));
GenSpeedFU  = GenSpeedF(1,:);
GenSpeedF   = str2num(GenSpeedF(2:end,:));

PitCom1U  = PitCom1(1,:);
PitCom1   = str2num(PitCom1(2:end,:));
PitRate1U  = PitRate1(1,:);
PitRate1   = str2num(PitRate1(2:end,:));
BlPitch1U  = BlPitch1(1,:);
BlPitch1   = str2num(BlPitch1(2:end,:));
PitComTU  = PitComT(1,:);
PitComT   = str2num(PitComT(2:end,:));

PitComIPC1U = PitComIPC1(1,:);
PitComIPC1  = str2num(PitComIPC1(2:end,:));
PitComIPCF1U = PitComIPCF1(1,:);
PitComIPCF1  = str2num(PitComIPCF1(2:end,:));

rootMOOP1U   = rootMOOP1(1,:);
rootMOOP1    = str2num(rootMOOP1(2:end,:));
rootMOOPF1U   = rootMOOPF1(1,:);
rootMOOPF1    = str2num(rootMOOPF1(2:end,:));

HorWindVU  = HorWindV(1,:);
HorWindV   = str2num(HorWindV(2:end,:));
% 
% GenTrqU  = AvrSWAP0x28470x29(1,:);
% GenTrq   = str2num(AvrSWAP0x28470x29(2:end,:))./10000;

aziAngle    = AvrSWAP0x28600x29(1,:);
aziAngle   = str2num(AvrSWAP0x28600x29(2:end,:));

%% Plotting
figure
title('GenSpeed')
hold on
plot(Time,GenSpeed)
plot(Time,GenSpeedF)
legend('GenSpeed','GenSpeedF')

% figure
% title('Pitch')
% hold on
% subplot(2,1,1), plot(Time,PitCom1)
% legend('PitCom1')
% subplot(2,1,2), plot(Time,PitRate1)
% legend('PitRate1')

figure
title('Pitch')
hold on
plot(Time,PitCom1)
plot(Time,PitRate1)
plot(Time,BlPitch1)
plot(Time,PitComT)
% plot(Time,GenTrq)
legend('PitCom1','PitRate1','BlPitch1','PitComT')

figure
title('rootMOOP')
hold on
plot(Time,rootMOOP1)
plot(Time,rootMOOPF1)
legend('rootMOOP1','rootMOOPF1')

figure
title('PitComIPC')
hold on
plot(Time,PitComIPC1)
plot(Time,PitComIPCF1)
legend('PitComIPC1','PitComIPCF1')

figure
title('WindSpeed')
plot(Time,HorWindV)
legend('HorWindV')

figure
title('PitComT')
hold on
plot(Time,PitComT)
plot(Time,PitComIPC1)
legend('PitComT','PitComIPC1')



%% FFT
% figure
% FFTrootMOOP1    = fft(rootMOOP1);
% Pyy             = FFTrootMOOP1.*conj(FFTrootMOOP1);
% % f               = 1000/251*(0:127);
% plot(Pyy(2:50))


