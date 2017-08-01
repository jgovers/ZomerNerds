load output\NREL5MW_out.mat

Fs = 125; % Sampling frequency [Hz]
T = 1/Fs; % Sampling period [s]
N = length(Time); % Amount of samples [-]

Y = fft(NcIMUTAxs); % Compute frequency response of fore-aft acceleration signal
% Y = fft(sin(Azimuth/180*pi)); % Compute frequency response of azimuth signal

P2 = abs(Y/N); % Take the magnitude of the frequency response
P1 = P2(1:N/2+1); % Only take the right side of the frequency response
P1(2:end-1) = 2*P1(2:end-1);

f = Fs*(0:(N/2))/N; % Make a frequency grid
plot(f,P1) 
title('Single-Sided Amplitude Spectrum of X(t)')
xlabel('f (Hz)')
ylabel('|P1(f)|')
xlim([0 2])
grid on