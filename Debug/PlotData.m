%% Cleaning
close all
clearvars
clc

%% Settings
TimeStamp = '2017_08_01_1548';
doAvrSwap = false;

%% Loading
[~, userprofile] = dos('echo %USERPROFILE%');
debugFolder = [userprofile(1:end-1) '\Dropbox\ZomerNerds\Debug\' TimeStamp '\'];
tdfread([debugFolder 'Test18.SrvD.dbg']);
if(doAvrSwap)
    tdfread([debugFolder 'Test18.SrvD.dbg2']);
end

%% Formatting
TimeU       = Time(1,:);
Time        = str2num(Time(2:end,:));

GenSpeedU   = GenSpeed(1,:);
GenSpeed    = str2num(GenSpeed(2:end,:));
GenSpeedFU  = GenSpeedF(1,:);
GenSpeedF   = str2num(GenSpeedF(2:end,:));

PitCom1U  = PitCom1(1,:);
PitCom1   = str2num(PitCom1(2:end,:));
PitCom2U  = PitCom2(1,:);
PitCom2   = str2num(PitCom2(2:end,:));
PitCom3U  = PitCom3(1,:);
PitCom3   = str2num(PitCom3(2:end,:));

PitRate1U  = PitRate1(1,:);
PitRate1   = str2num(PitRate1(2:end,:));
PitRate2U  = PitRate2(1,:);
PitRate2   = str2num(PitRate2(2:end,:));
PitRate3U  = PitRate3(1,:);
PitRate3   = str2num(PitRate3(2:end,:));

BlPitch1U  = BlPitch1(1,:);
BlPitch1   = str2num(BlPitch1(2:end,:));

PitComT1U  = PitComT1(1,:);
PitComT1   = str2num(PitComT1(2:end,:));
PitComT2U  = PitComT2(1,:);
PitComT2   = str2num(PitComT2(2:end,:));
PitComT3U  = PitComT3(1,:);
PitComT3   = str2num(PitComT3(2:end,:));

PitComIPC1U = PitComIPC1(1,:);
PitComIPC1  = str2num(PitComIPC1(2:end,:));
PitComIPCF1U = PitComIPCF1(1,:);
PitComIPCF1  = str2num(PitComIPCF1(2:end,:));
PitComIPCF2U = PitComIPCF2(1,:);
PitComIPCF2  = str2num(PitComIPCF2(2:end,:));
PitComIPCF3U = PitComIPCF3(1,:);
PitComIPCF3  = str2num(PitComIPCF3(2:end,:));

rootMOOP1U   = rootMOOP1(1,:);
rootMOOP1    = str2num(rootMOOP1(2:end,:));
rootMOOPF1U   = rootMOOPF1(1,:);
rootMOOPF1    = str2num(rootMOOPF1(2:end,:));

rootMOOP2U   = rootMOOP2(1,:);
rootMOOP2    = str2num(rootMOOP2(2:end,:));
rootMOOPF2U   = rootMOOPF2(1,:);
rootMOOPF2    = str2num(rootMOOPF2(2:end,:));

rootMOOP3U   = rootMOOP3(1,:);
rootMOOP3    = str2num(rootMOOP3(2:end,:));
rootMOOPF3U   = rootMOOPF3(1,:);
rootMOOPF3    = str2num(rootMOOPF3(2:end,:));

HorWindVU  = HorWindV(1,:);
HorWindV   = str2num(HorWindV(2:end,:));
% 
% GenTrqU  = AvrSWAP0x28470x29(1,:);
% GenTrq   = str2num(AvrSWAP0x28470x29(2:end,:))./10000;

% aziAngle    = AvrSWAP0x28600x29(1,:);
% aziAngle   = str2num(AvrSWAP0x28600x29(2:end,:));

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
plot(Time,PitComT1)
% plot(Time,GenTrq)
legend('PitCom1','PitRate1','BlPitch1','PitComT')

figure
title('rootMOOP')
hold on
plot(Time,rootMOOP1)
plot(Time,rootMOOPF1)
plot(Time,rootMOOP2)
plot(Time,rootMOOPF2)
plot(Time,rootMOOP3)
plot(Time,rootMOOPF3)
legend('rootMOOP1','rootMOOPF1','rootMOOP2','rootMOOPF2','rootMOOP3','rootMOOPF3')

figure
title('PitComIPC')
hold on
plot(Time,PitComIPCF1)
plot(Time,PitComIPCF2)
plot(Time,PitComIPCF3)
legend('PitComIPCF1','PitComIPCF2','PitComIPCF3')

figure
title('WindSpeed')
plot(Time,HorWindV)
legend('HorWindV')

figure
title('PitComT')
hold on
plot(Time,PitComT1)
plot(Time,PitComT2)
plot(Time,PitComT3)
legend('PitComT1','PitComT2','PitComT3')

figure
title('PitRate')
hold on
plot(Time,PitRate1)
plot(Time,PitRate2)
plot(Time,PitRate3)
legend('PitRate1','PiRate2','PitRate3')

figure
title('PitCom')
hold on
plot(Time,PitCom1)
plot(Time,PitCom2)
plot(Time,PitCom3)
legend('PitCom1','PitCom2','PitCom3')




%% FFT
% figure
% FFTrootMOOP1    = fft(rootMOOP1);
% Pyy             = FFTrootMOOP1.*conj(FFTrootMOOP1);
% % f               = 1000/251*(0:127);
% plot(Pyy(2:50))


