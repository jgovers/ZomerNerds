%% Cleaning
close all
clearvars
clc

%% Settings
totalTime = tic;
timeStamp = 'rc';               % set to 'rc' to just get the most recent folder
testFile = 'Test18.SL.out';      % Name of the test file to read
runCmdFromHere = false;          % Run the CompileRunAndDebug.cmd file from this matlab script
saveAllFigures = true;          % Automatically save all figures in the debug folder

%% Loading
    if(runCmdFromHere)  % Run CompileRunAndDebug.cmd and get the correct folder
        [~,output] = dos('..\RunAndDebugSimulink.cmd', '-echo');
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
        
    outRaw = dlmread([debugFolder testFile],'\t',8,0);
    [~,vars] = size(outRaw);
    fid = fopen([debugFolder testFile]);
    header = textscan(fid,'%s','delimiter','\t');
    fclose(fid);
    header = strtrim(header{1,1}(4:vars+3));
    for i = 1:vars
        out.(header{i}) = outRaw(:,i);
    end

%% Plotting
% calculate total wind
Wind1VelTot = sqrt(out.Wind1VelX.^2+out.Wind1VelY.^2+out.Wind1VelZ.^2);

figure;
title('Wind Velocity')
hold on
plot(out.Time,out.Wind1VelX)
plot(out.Time,out.Wind1VelY)
plot(out.Time,out.Wind1VelZ)
plot(out.Time,Wind1VelTot)
ylabel('Wind velocity [m/s]')
legend('x','y','z','total')

figure;
title('Rotor and Generator Speed')
hold on
plot(out.Time,out.RotSpeed)
ylabel('Rotor speed [rpm]')
yyaxis right
plot(out.Time,out.GenSpeed)
ylabel('Generator speed [rpm]')

figure;
title('Generator Torque')
hold on
plot(out.Time,out.GenTq)
ylabel('Torque [kN*m]')

figure;
title('BldPitch1')
hold on
plot(out.Time,out.BldPitch1)
ylabel('Pitch [deg]')

% Calculate RootMtotb1
RootMtotb1 = sqrt(out.RootMxb1.^2 + out.RootMyb1.^2 + out.RootMzb1.^2);

figure
title('Blade 1 root out of plane bending moment')
hold on
plot(out.Time,out.RootMyb1)
ylabel('Moment [kN*m]')

%% Save figures
if(saveAllFigures)
    figArray=findall(0,'type','figure');
    for i = 1:length(figArray)
        figure(figArray(i).Number)
        saveas(figArray(i),[debugFolder 'fig' get(get(gca,'title'),'string') '.fig']);
        saveas(figArray(i),[debugFolder 'fig' get(get(gca,'title'),'string') '.png']);
    end
    disp(['Saved all figures to ' debugFolder(1:end-1)]);
end

disp(['Folder: ' debugFolder])
toc(totalTime)
