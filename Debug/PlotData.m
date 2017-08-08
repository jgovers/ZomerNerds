%% Cleaning
close all
clearvars
clc

%% Settings
TimeStamp = '2017_08_07\2017_08_07_1739';
doAvrSwap = true;              % Read the AvrSwap debug file
runCmdFromHere = true;          % Run the CompileRunAndDebug.cmd file from this matlab script
saveAllFigures = false;          % Automatically save all figures in the debug folder

%% Loading
if(runCmdFromHere)  % Run CompileRunAndDebug.cmd and get the correct folder
    [~,output] = dos('..\CompileRunAndDebug.cmd', '-echo');
    index = strfind(output,'C:');
    index = index(end);
    debugFolder = [output(index:end-1) '\'];
    disp(['debugFolder: ' debugFolder]);
else                % otherwise get the debugfolder with the manual timestamp
    [~, userprofile] = dos('echo %USERPROFILE%');
    debugFolder = [userprofile(1:end-1) '\Dropbox\ZomerNerds\Debug\' TimeStamp '\'];
end


tic
tdfread([debugFolder 'Test18.SrvD.dbg']);

if(doAvrSwap)
    AvrSWAP = dlmread([debugFolder 'Test18.SrvD.dbg2'],'\t',8,0);
    AvrTime = AvrSWAP(:,1);
    AvrSWAP = AvrSWAP(:,2:end);
end
toc

%% Formatting debugfile
TimeU       = Time(1,:);
Time        = str2num(Time(2:end,:)); %#ok<*ST2NM> suppresses all warnings about str2double

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

% PitComTU  = PitComT(1,:);
% PitComT   = str2num(PitComT(2:end,:));

% PitComIPC1U = PitComIPC1(1,:);
% PitComIPC1  = str2num(PitComIPC1(2:end,:));
PitComIPCF1U = PitComIPCF1(1,:);
PitComIPCF1  = str2num(PitComIPCF1(2:end,:));
% PitComIPCF2U = PitComIPCF2(1,:);
% PitComIPCF2  = str2num(PitComIPCF2(2:end,:));
% PitComIPCF3U = PitComIPCF3(1,:);
% PitComIPCF3  = str2num(PitComIPCF3(2:end,:));

rootMOOP1U   = rootMOOP1(1,:);
rootMOOP1    = str2num(rootMOOP1(2:end,:));
% rootMOOPF1U   = rootMOOPF1(1,:);
% rootMOOPF1    = str2num(rootMOOPF1(2:end,:));

% rootMOOP2U   = rootMOOP2(1,:);
% rootMOOP2    = str2num(rootMOOP2(2:end,:));
% rootMOOPF2U   = rootMOOPF2(1,:);
% rootMOOPF2    = str2num(rootMOOPF2(2:end,:));

% rootMOOP3U   = rootMOOP3(1,:);
% rootMOOP3    = str2num(rootMOOP3(2:end,:));
% rootMOOPF3U   = rootMOOPF3(1,:);
% rootMOOPF3    = str2num(rootMOOPF3(2:end,:));

HorWindVU  = HorWindV(1,:);
HorWindV   = str2num(HorWindV(2:end,:));
% 
% GenTrqU  = AvrSWAP0x28470x29(1,:);
% GenTrq   = str2num(AvrSWAP0x28470x29(2:end,:))./10000;

% aziAngle    = AvrSWAP0x28600x29(1,:);
% aziAngle   = str2num(AvrSWAP0x28600x29(2:end,:));

Y_MErrU             = Y_MErr(1,:);
Y_MErr              = str2num(Y_MErr(2:end,:));
Y_ErrLPFFastU       = Y_ErrLPFFast(1,:);
Y_ErrLPFFast        = str2num(Y_ErrLPFFast(2:end,:));
Y_ErrLPFSlowU    = Y_ErrLPFSlow(1,:);
Y_ErrLPFSlow     = str2num(Y_ErrLPFSlow(2:end,:));

Y_AccErrU    = Y_AccErr(1,:);
Y_AccErr     = str2num(Y_AccErr(2:end,:));

YawTestU    = YawTest(1,:);
YawTest     = str2num(YawTest(2:end,:));    


%% Plotting
figure
title('GenSpeed')
hold on
plot(Time,GenSpeed)
plot(Time,GenSpeedF)
legend('GenSpeed','GenSpeedF')
yaxis('Speed [rpm]')

figure
title('GenTorque')
hold on
plot(AvrTime,AvrSWAP(:,47))

% figure
% title('Pitch')
% hold on
% subplot(2,1,1), plot(Time,PitCom1)
% legend('PitCom1')
% subplot(2,1,2), plot(Time,PitRate1)
% legend('PitRate1')
% 
figure
title('Pitch')
hold on
plot(Time,PitCom1)
% plot(Time,PitRate1)
plot(Time,BlPitch1)
plot(Time,PitComT1)
% plot(Time,PitComT)
% plot(Time,GenTrq)
legend('PitCom1','BlPitch1','PitComT')

figure
title('rootMOOP')
hold on
plot(Time,rootMOOP1)
% plot(Time,rootMOOPF1)
% plot(Time,rootMOOP2)
% % plot(Time,rootMOOPF2)
% plot(Time,rootMOOP3)
% plot(Time,rootMOOPF3)
legend('rootMOOP1')

figure
title('PitComIPC')
hold on
plot(Time,PitComIPCF1)
% plot(Time,PitComIPCF2)
% plot(Time,PitComIPCF3)
legend('PitComIPCF1')
% 
figure
title('WindVelocity')
hold on
plot(Time,HorWindV)
legend('HorWindV')

% figure
% title('PitComT')
% hold on
% plot(Time,PitComT1)
% plot(Time,PitComT2)
% plot(Time,PitComT3)
% legend('PitComT1','PitComT2','PitComT3')
% 
% figure
% title('PitRate')
% hold on
% plot(Time,PitRate1)
% plot(Time,PitRate2)
% plot(Time,PitRate3)
% legend('PitRate1','PiRate2','PitRate3')
% 
% figure
% title('PitCom')
% hold on
% plot(Time,PitCom1)
% plot(Time,PitCom2)
% plot(Time,PitCom3)
% legend('PitCom1','PitCom2','PitCom3')
% 
% figure
% title('Measured yaw error')
% hold on
% plot(Time,Y_MErr)
% plot(Time,Y_ErrLPFFast)
% plot(Time,Y_ErrLPFSlow)
% legend('Y MErr','Y ErrLPFFast','Y ErrLPFSlow')
% 
% figure
% title('Integral of fast yaw error')
% hold on
% grid on
% plot(Time,Y_ErrLPFFast)
% plot(Time,Y_AccErr)
% plot(Time,YawTest)
% legend('Y ErrLPFFast','Y AccErr','YawTest')
% 
% figure
% title('YawTest')
% hold on
% grid on
% plot(Time,YawTest)
% legend('YawTest')

%% FFT
% figure
% FFTrootMOOP1    = fft(rootMOOP1);
% Pyy             = FFTrootMOOP1.*conj(FFTrootMOOP1);
% % f               = 1000/251*(0:127);
% plot(Pyy(2:50))

%% Save figures
if(saveAllFigures)
    figArray=findall(0,'type','figure');
    for i = 1:length(figArray)
        figure(figArray(i).Number)
        saveas(figArray(i),[debugFolder 'fig' get(get(gca,'title'),'string') '.fig']);
    end
    disp(['Saved all figures to ' debugFolder(1:end-1)]);
end


