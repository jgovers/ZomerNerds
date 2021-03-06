%% Cleaning
close all
clearvars
clc

%% Settings
timeStamp = '2017_08_17_1030';               % set to 'rc' to just get the most recent folder
doAvrSwap = true;              % Read the avrSWAP debug file
runCmdFromHere = false;          % Run the CompileRunAndDebug.cmd file from this matlab script
saveAllFigures = false;          % Automatically save all figures in the debug folder

%% Loading
totalTime = tic;
if(runCmdFromHere)  % Run CompileRunAndDebug.cmd and get the correct folder
    [~,output] = dos('..\CompileRunAndDebug.cmd', '-echo');
    i = strfind(output,'C:');
    i = i(end);
    debugFolder = [output(i:end-1) '\'];
    clearvars index output TimeStamp
else                % otherwise get the debugfolder with the manual timestamp
    [~, userprofile] = dos('echo %USERPROFILE%');
    debugFolder = [userprofile(1:end-1) '\Dropbox\ZomerNerds\Debug\'];
    if strcmp(timeStamp,'rc')   % if the timeStamp is rc, search for the most recent folder
        d = dir(debugFolder);
        [~,order] = sort([d.datenum]);
        timeStamp = d(order==1).name;
    end
    debugFolder = [debugFolder timeStamp '\'];
    clearvars userprofile d order
end


% dbRaw = tdfread([debugFolder 'Test18.SrvD.dbg']);
dbRaw = dlmread([debugFolder 'Test18.SrvD.dbg'],'\t',8,0);
[~,vars] = size(dbRaw);
fid = fopen([debugFolder 'Test18.SrvD.dbg']);
header = textscan(fid,'%s','delimiter','\t');
fclose(fid);
header = strtrim(header{1,1}(1:vars));
for i = 1:vars
    db.(header{i}) = dbRaw(:,i);
end

if(doAvrSwap)
    avrSWAP = dlmread([debugFolder 'Test18.SrvD.dbg2'],'\t',8,0);
    avrTime = avrSWAP(:,1);
    avrSWAP = avrSWAP(:,2:end);
end


%% Plotting
figure
title('GenSpeed')
hold on
plot(db.Time,db.GenSpeed)
plot(db.Time,db.GenSpeedF)
legend('GenSpeed','GenSpeedF')
ylabel('Speed [rpm]')

figure
title('GenTorque')
hold on
plot(avrTime,avrSWAP(:,47))

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
plot(db.Time,db.PitCom1)
% plot(Time,PitRate1)
plot(db.Time,db.BlPitch1)
plot(db.Time,db.PitComT1)
% plot(Time,PitComT)
% plot(Time,GenTrq)
legend('PitCom1','BlPitch1','PitComT')

figure
title('rootMOOP')
hold on
plot(db.Time,db.rootMOOP1)
% plot(Time,rootMOOPF1)
% plot(Time,rootMOOP2)
% % plot(Time,rootMOOPF2)
% plot(Time,rootMOOP3)
% plot(Time,rootMOOPF3)
legend('rootMOOP1')

figure
title('PitComIPC')
hold on
plot(db.Time,db.PitComF1)
% plot(Time,PitComIPCF2)
% plot(Time,PitComIPCF3)
legend('PitComIPCF1')
% 
figure
title('WindVelocity')
hold on
plot(db.Time,db.HorWindV)
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
plot(db.Time,db.MErr)
plot(db.Time,db.ErrLPFFast)
plot(db.Time,db.ErrLPFSlow)
plot(db.Time,rad2deg(avrSWAP(:,37)))
legend('Y MErr','Y ErrLPFFast','Y ErrLPFSlow','Turibine yaw')

figure
title('Integral of fast yaw error')
hold on
grid on
plot(db.Time,db.ErrLPFFast)
plot(db.Time,db.Y_AccErr)
legend('Y ErrLPFFast','Y AccErr')

% figure
% title('Yaw Rate')
% hold on
% grid on
% plot(db.Time,avrSWAP(:,48))
% legend('Yaw Rate')

figure
title('YawTorque')
hold on
grid on
plot(avrTime,avrSWAP(:,41))
legend('YawTorque')

figure
title('Yaw end time')
hold on
plot(db.Time,db.Y_YawEndT)
plot(db.Time,db.Time)
legend('YawEndT','Time')

%% Other figures
figure;
title('Wind Velocity')
hold on
plot(db.Time,db.HorWindV)
ylabel('Wind velocity [m/s]')
legend('x')

figure;
title('Rotor and Generator Speed')
hold on
plot(db.Time,avrSWAP(:,21))
ylabel('Rotor speed [rpm]')
yyaxis right
plot(db.Time,db.GenSpeed)
ylabel('Generator speed [rpm]')

figure;
title('Generator Torque')
hold on
plot(db.Time,avrSWAP(:,47))
ylabel('Torque [kN*m]')

figure;
title('BldPitch1')
hold on
plot(db.Time,db.BlPitch1)
ylabel('Pitch [deg]')


% RootMtotb1 = sqrt(out.RootMxb1.^2 + out.RootMyb1.^2 + out.RootMzb1.^2);
% 
% figure
% title('Blade 1 root bending moments')
% hold on
% plot(out.Time,out.RootMxb1)
% plot(out.Time,out.RootMyb1)
% plot(out.Time,out.RootMzb1)
% plot(out.Time,RootMtotb1)
% ylabel('Moment [kN*m]')
% legend('x','y','z','total')

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

disp(['Folder: ' debugFolder])
toc(totalTime)
