%% Cleaning
close all
clearvars
clc

%% Settings
TimeStamp = '2017_08_08_1147';
doAvrSwap = true;              % Read the AvrSwap debug file
runCmdFromHere = false;          % Run the CompileRunAndDebug.cmd file from this matlab script
saveAllFigures = false;          % Automatically save all figures in the debug folder

%% Loading
if(runCmdFromHere)  % Run CompileRunAndDebug.cmd and get the correct folder
    [~,output] = dos('..\CompileRunAndDebug.cmd', '-echo');
    index = strfind(output,'C:');
    index = index(end);
    debugFolder = [output(index:end-1) '\'];
    disp(['debugFolder: ' debugFolder]);
    clearvars index output TimeStamp
else                % otherwise get the debugfolder with the manual timestamp
    [~, userprofile] = dos('echo %USERPROFILE%');
    debugFolder = [userprofile(1:end-1) '\Dropbox\ZomerNerds\Debug\' TimeStamp '\'];
    clearvars userprofile
end


dbStruct = tdfread([debugFolder 'Test18.SrvD.dbg']);

if(doAvrSwap)
    AvrSWAP = dlmread([debugFolder 'Test18.SrvD.dbg2'],'\t',8,0);
    AvrTime = AvrSWAP(:,1);
    AvrSWAP = AvrSWAP(:,2:end);
end

%% Formatting debugfile

Time        = str2num(dbStruct.Time(2:end,:)); %#ok<*ST2NM> suppresses all warnings about str2double

GenSpeed    = str2num(dbStruct.GenSpeed(2:end,:));
GenSpeedF   = str2num(dbStruct.GenSpeedF(2:end,:));

PitCom1   = str2num(dbStruct.PitCom1(2:end,:));
PitCom2   = str2num(dbStruct.PitCom2(2:end,:));
PitCom3   = str2num(dbStruct.PitCom3(2:end,:));

PitRate1   = str2num(dbStruct.PitRate1(2:end,:));
PitRate2   = str2num(dbStruct.PitRate2(2:end,:));
PitRate3   = str2num(dbStruct.PitRate3(2:end,:));

BlPitch1   = str2num(dbStruct.BlPitch1(2:end,:));

PitComT1   = str2num(dbStruct.PitComT1(2:end,:));
PitComT2   = str2num(dbStruct.PitComT2(2:end,:));
PitComT3   = str2num(dbStruct.PitComT3(2:end,:));

% PitComT   = str2num(dbStruct.PitComT(2:end,:));

% PitComIPC1  = str2num(dbStruct.PitComIPC1(2:end,:));
PitComIPCF1  = str2num(dbStruct.PitComIPCF1(2:end,:));
% PitComIPCF2  = str2num(dbStruct.PitComIPCF2(2:end,:));
% PitComIPCF3  = str2num(dbStruct.PitComIPCF3(2:end,:));

rootMOOP1    = str2num(dbStruct.rootMOOP1(2:end,:));
% rootMOOPF1    = str2num(dbStruct.rootMOOPF1(2:end,:));

% rootMOOP2    = str2num(dbStruct.rootMOOP2(2:end,:));
% rootMOOPF2    = str2num(dbStruct.rootMOOPF2(2:end,:));

% rootMOOP3    = str2num(dbStruct.rootMOOP3(2:end,:));
% rootMOOPF3    = str2num(dbStruct.rootMOOPF3(2:end,:));

HorWindV   = str2num(dbStruct.HorWindV(2:end,:));
% 
% GenTrq   = str2num(dbStruct.AvrSWAP0x28470x29(2:end,:))./10000;

% aziAngle   = str2num(dbStruct.AvrSWAP0x28600x29(2:end,:));

Y_MErr              = str2num(dbStruct.Y_MErr(2:end,:));

Y_ErrLPFFast        = str2num(dbStruct.Y_ErrLPFFast(2:end,:));

Y_ErrLPFSlow     = str2num(dbStruct.Y_ErrLPFSlow(2:end,:));

Y_AccErr     = str2num(dbStruct.Y_AccErr(2:end,:));

YawTest     = str2num(dbStruct.YawTest(2:end,:));

Y_YawEndT   = str2num(dbStruct.Y_YawEndT(2:end,:));


%% Plotting
figure
title('GenSpeed')
hold on
plot(Time,GenSpeed)
plot(Time,GenSpeedF)
legend('GenSpeed','GenSpeedF')
ylabel('Speed [rpm]')

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
figure
title('Measured yaw error')
hold on
plot(Time,Y_MErr)
plot(Time,Y_ErrLPFFast)
plot(Time,Y_ErrLPFSlow)
legend('Y MErr','Y ErrLPFFast','Y ErrLPFSlow')

figure
title('Integral of fast yaw error')
hold on
grid on
plot(Time,Y_ErrLPFFast)
plot(Time,Y_AccErr)
plot(Time,YawTest)
legend('Y ErrLPFFast','Y AccErr','YawTest')

figure
title('YawTest')
hold on
grid on
plot(Time,YawTest)
legend('YawTest')

figure
title('Yaw end time')
hold on
plot(Time,Y_YawEndT)
plot(Time,Time)
legend('Y_YawEndT','Time')

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


