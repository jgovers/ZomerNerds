%% Cleaning
close all
clearvars
clc

%% Settings
TimeStamp = '2017_08_02_1125';
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
PitRate1U  = PitRate1(1,:);
PitRate1   = str2num(PitRate1(2:end,:));
BlPitch1U  = BlPitch1(1,:);
BlPitch1   = str2num(BlPitch1(2:end,:));
PitComTU  = PitComT(1,:);
PitComT   = str2num(PitComT(2:end,:));

HorWindVU  = HorWindV(1,:);
HorWindV   = str2num(HorWindV(2:end,:));

if(doAvrSwap)
    GenTrqU  = AvrSWAP0x28470x29(1,:);
    GenTrq   = str2num(AvrSWAP0x28470x29(2:end,:))./10000;
end

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
plot(Time,GenTrq)
legend('PitCom1','PitRate1','BlPitch1','PitComT','GenTrq')

figure
title('WindSpeed')
plot(Time,HorWindV)
legend('HorWindV')


