% load output\NREL5MW_out.mat

T = 0.00625; % Sampling period [s]
Fs = 1/T; % Sampling frequency [Hz]
N = length(Time); % Amount of samples [-]

Y = fft(rootMOOPF1-mean(rootMOOPF1)); % Compute frequency response of fore-aft acceleration signal

% Y = fft(sin(aziAngle)); % Compute frequency response of azimuth signal

P2 = abs(Y/N); % Take the magnitude of the frequency response
P1 = P2(1:N/2+1); % Only take the right side of the frequency response
P1(2:end-1) = 2*P1(2:end-1);

f = Fs*(0:(N/2))/N; % Make a frequency grid

figure
plot(f,P1) 
title('Single-Sided Amplitude Spectrum of X(t)')
xlabel('f (Hz)')
ylabel('|P1(f)|')
xlim([0.1 1])
grid on