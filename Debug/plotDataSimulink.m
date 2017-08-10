%% Cleaning
close all
clearvars
clc

%% Settings
totalTime = tic;
timeStamp = 'rc';               % set to 'rc' to just get the most recent folder
runCmdFromHere = false;          % Run the CompileRunAndDebug.cmd file from this matlab script
saveAllFigures = false;          % Automatically save all figures in the debug folder

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
        
    outRaw = dlmread([debugFolder 'Test18.SL.out'],'\t',8,0);
    [~,vars] = size(outRaw);
    fid = fopen([debugFolder 'Test18.SL.out']);
    header = textscan(fid,'%s','delimiter','\t');
    fclose(fid);
    header = strtrim(header{1,1}(4:vars+3));
    for i = 1:vars
        out.(header{i}) = outRaw(:,i);
    end

%% Plotting
figure;
title('Wind Velocity')
hold on
plot(out.Time,out.Wind1VelX)
ylabel('Wind velocity [m/s]')

figure;
title('RotSpeed')
hold on
plot(out.Time,out.RotSpeed)
ylabel('Rotor speed [rpm]')

figure;
title('Generator Torque')
hold on
plot(out.Time,out.GenTq)
ylabel('Torque [kN*m]')

figure;
title('Generator Speed')
hold on
plot(out.Time,out.GenSpeed)
ylabel('Generator speed [rpm]')

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
