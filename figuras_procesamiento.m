clear; clc; close all;

%% =========================
%  CONFIGURACIÓN GENERAL
% ==========================
base_path = 'C:\Users\jguer\Dropbox\linea_investigacion\data\cocontraccion';

files.NN   = fullfile(base_path, 'test', 'test_norm', 'test_norm_jesseavilez.mat');
files.aHH  = fullfile(base_path, 'test', 'test_a',    'test_hyp_jesseavilez.mat');
files.HH48 = fullfile(base_path, 'test', 'test_c',    'test_hyp_c_jesseavilez.mat');

out_path = fullfile(base_path, 'figuras_jesseavilez');
if ~exist(out_path, 'dir')
    mkdir(out_path);
end

% Nombres de canales
nombre_triceps = 'RT LAT. TRICEPS, uV';
nombre_biceps  = 'RT BICEPS BR., uV';

% Parámetros del pipeline
hp_cutoff = 20;    % Hz
lp_cutoff = 70;    % Hz
fs_target = 200;   % Hz
segment_duration_sec = 2.5;
nfft = 512;
window = hamming(256);
noverlap = 128;
alpha_sig = 0.05;

% Bandas
bands = {
    'Delta', [1 4];
    'Theta', [4 8];
    'Alpha', [8 12];
    'Beta',  [13 30];
    'Gamma', [30 45];
};

% Tramos para figuras
segment_plot_start_fig1 = 40;  % s
segment_plot_end_fig1   = 60;  % s

segment_plot_start_fig2 = 40;  % s
segment_plot_end_fig2   = 60;  % s

% Etiquetas condiciones
conds = {'NN','aHH','HH48'};
titles_fig = {'NN', 'aHH', '48-h HH'};

% Colores fijos para coherencia
col_NN   = [0 0.4470 0.7410];
col_aHH  = [0.8500 0.3250 0.0980];
col_HH48 = [0.4660 0.6740 0.1880];
col_thr  = [0.25 0.25 0.25];

%% =========================
%  PROCESAR LAS 3 CONDICIONES
% ==========================
results = struct();

for c = 1:numel(conds)
    cond = conds{c};
    fprintf('Procesando condición: %s\n', cond);

    results.(cond) = process_emg_and_coherence( ...
        files.(cond), ...
        nombre_triceps, ...
        nombre_biceps, ...
        hp_cutoff, ...
        lp_cutoff, ...
        fs_target, ...
        segment_duration_sec, ...
        window, ...
        noverlap, ...
        nfft, ...
        alpha_sig);
end

%% =========================
%  FIGURA 1: SEÑALES CRUDAS (40-60 s)
% ==========================
fig1 = figure('Color', 'w', 'Position', [100 100 1200 900]);

for c = 1:numel(conds)
    cond = conds{c};
    R = results.(cond);

    fs_raw = R.fs_raw;
    idx1 = max(1, round(segment_plot_start_fig1 * fs_raw));
    idx2 = min(length(R.triceps_raw), round(segment_plot_end_fig1 * fs_raw));
    t = (idx1:idx2) ./ fs_raw;

    y1 = R.triceps_raw(idx1:idx2);
    y2 = R.biceps_raw(idx1:idx2);

    % ampliar un poco el eje Y
    y_all = [y1(:); y2(:)];
    y_min = min(y_all);
    y_max = max(y_all);
    y_pad = 0.10 * (y_max - y_min + eps);

    subplot(3,1,c);
    plot(t, y1, 'LineWidth', 1); hold on;
    plot(t, y2, 'LineWidth', 1);
    xlim([segment_plot_start_fig1 segment_plot_end_fig1]);
    ylim([y_min - y_pad, y_max + y_pad]);
    xlabel('Time (s)');
    ylabel('EMG (\muV)');
    title(titles_fig{c});
    legend({'Triceps', 'Biceps'}, 'Location', 'southeast');
    grid on;
end

saveas(fig1, fullfile(out_path, 'Figure_1_Raw_EMG.png'));

%% =========================
%  FIGURA 2: PIPELINE DE PROCESAMIENTO (40-60 s)
%  5 filas x 3 columnas
% ==========================
fig2 = figure('Color', 'w', 'Position', [100 50 1600 1000]);

for c = 1:numel(conds)
    cond = conds{c};
    R = results.(cond);

    % Índices raw para 40-60 s
    idx1_raw = max(1, round(segment_plot_start_fig2 * R.fs_raw));
    idx2_raw = min(length(R.triceps_raw), round(segment_plot_end_fig2 * R.fs_raw));
    t_raw = (idx1_raw:idx2_raw) ./ R.fs_raw;

    % Índices ds para 40-60 s
    idx1_ds = max(1, round(segment_plot_start_fig2 * R.fs_ds));
    idx2_ds = min(length(R.triceps_norm), round(segment_plot_end_fig2 * R.fs_ds));
    t_ds = (idx1_ds:idx2_ds) ./ R.fs_ds;

    % fila 1: raw
    subplot(5,3,c);
    plot(t_raw, R.triceps_raw(idx1_raw:idx2_raw), 'LineWidth', 1); hold on;
    plot(t_raw, R.biceps_raw(idx1_raw:idx2_raw), 'LineWidth', 1);
    xlim([segment_plot_start_fig2 segment_plot_end_fig2]);
    title(titles_fig{c});
    if c == 1, ylabel('Raw (\muV)'); end
    if c == 3, legend({'Triceps', 'Biceps'}, 'Location', 'best'); end
    grid on;

    % fila 2: high-pass
    subplot(5,3,3 + c);
    plot(t_raw, R.triceps_hp(idx1_raw:idx2_raw), 'LineWidth', 1); hold on;
    plot(t_raw, R.biceps_hp(idx1_raw:idx2_raw), 'LineWidth', 1);
    xlim([segment_plot_start_fig2 segment_plot_end_fig2]);
    if c == 1, ylabel('HP 20 Hz'); end
    grid on;

    % fila 3: envelope
    subplot(5,3,6 + c);
    plot(t_raw, R.triceps_env(idx1_raw:idx2_raw), 'LineWidth', 1); hold on;
    plot(t_raw, R.biceps_env(idx1_raw:idx2_raw), 'LineWidth', 1);
    xlim([segment_plot_start_fig2 segment_plot_end_fig2]);
    if c == 1, ylabel('Envelope'); end
    grid on;

    % fila 4: low-pass filtered envelope
    subplot(5,3,9 + c);
    plot(t_raw, R.triceps_filt(idx1_raw:idx2_raw), 'LineWidth', 1); hold on;
    plot(t_raw, R.biceps_filt(idx1_raw:idx2_raw), 'LineWidth', 1);
    xlim([segment_plot_start_fig2 segment_plot_end_fig2]);
    if c == 1, ylabel('LP 70 Hz'); end
    grid on;

    % fila 5: normalized resampled
    subplot(5,3,12 + c);
    plot(t_ds, R.triceps_norm(idx1_ds:idx2_ds), 'LineWidth', 1); hold on;
    plot(t_ds, R.biceps_norm(idx1_ds:idx2_ds), 'LineWidth', 1);
    xlim([segment_plot_start_fig2 segment_plot_end_fig2]);
    if c == 1, ylabel('Normalized'); end
    xlabel('Time (s)');
    grid on;
end

saveas(fig2, fullfile(out_path, 'Figure_2_Processing_Pipeline_40_60s.png'));

%% =========================
%  FIGURA 3-4 COMBINADA:
%  espectro completo + zooms por banda + una sola leyenda
% ==========================
fig34 = figure('Color', 'w', 'Position', [80 60 1500 950]);

% -------- Panel 1: espectro completo --------
subplot(3,2,1); hold on;

yl_top = 1;
band_colors = [
    0.92 0.92 0.92
    0.88 0.88 0.88
    0.84 0.84 0.84
    0.80 0.80 0.80
    0.76 0.76 0.76
];

for b = 1:size(bands,1)
    xr = bands{b,2};
    patch([xr(1) xr(2) xr(2) xr(1)], [0 0 yl_top yl_top], ...
        band_colors(b,:), 'FaceAlpha', 0.15, 'EdgeColor', 'none');
end

h1 = plot(results.NN.f,   results.NN.C_squared,  'Color', col_NN,   'LineWidth', 1.8);
h2 = plot(results.aHH.f,  results.aHH.C_squared, 'Color', col_aHH,  'LineWidth', 1.8);
h3 = plot(results.HH48.f, results.HH48.C_squared,'Color', col_HH48, 'LineWidth', 1.8);
h4 = yline(results.HH48.S_threshold, '--', 'Color', col_thr, 'LineWidth', 1.2);

xlim([0 50]);
ylim([0 yl_top]);
xlabel('Frequency (Hz)');
ylabel('Magnitude-squared coherence (a.u.)');
title('Full spectrum');
grid on;

% -------- Panel 2: Delta --------
subplot(3,2,2); hold on;
plot(results.NN.f,   results.NN.C_squared,  'Color', col_NN,   'LineWidth', 1.6);
plot(results.aHH.f,  results.aHH.C_squared, 'Color', col_aHH,  'LineWidth', 1.6);
plot(results.HH48.f, results.HH48.C_squared,'Color', col_HH48, 'LineWidth', 1.6);
yline(results.HH48.S_threshold, '--', 'Color', col_thr, 'LineWidth', 1.0);
xlim(bands{1,2});
xlabel('Frequency (Hz)');
ylabel('Coherence (a.u.)');
title(sprintf('%s (%g-%g Hz)', bands{1,1}, bands{1,2}(1), bands{1,2}(2)));
grid on;

% -------- Panel 3: Theta --------
subplot(3,2,3); hold on;
plot(results.NN.f,   results.NN.C_squared,  'Color', col_NN,   'LineWidth', 1.6);
plot(results.aHH.f,  results.aHH.C_squared, 'Color', col_aHH,  'LineWidth', 1.6);
plot(results.HH48.f, results.HH48.C_squared,'Color', col_HH48, 'LineWidth', 1.6);
yline(results.HH48.S_threshold, '--', 'Color', col_thr, 'LineWidth', 1.0);
xlim(bands{2,2});
xlabel('Frequency (Hz)');
ylabel('Coherence (a.u.)');
title(sprintf('%s (%g-%g Hz)', bands{2,1}, bands{2,2}(1), bands{2,2}(2)));
grid on;

% -------- Panel 4: Alpha --------
subplot(3,2,4); hold on;
plot(results.NN.f,   results.NN.C_squared,  'Color', col_NN,   'LineWidth', 1.6);
plot(results.aHH.f,  results.aHH.C_squared, 'Color', col_aHH,  'LineWidth', 1.6);
plot(results.HH48.f, results.HH48.C_squared,'Color', col_HH48, 'LineWidth', 1.6);
yline(results.HH48.S_threshold, '--', 'Color', col_thr, 'LineWidth', 1.0);
xlim(bands{3,2});
xlabel('Frequency (Hz)');
ylabel('Coherence (a.u.)');
title(sprintf('%s (%g-%g Hz)', bands{3,1}, bands{3,2}(1), bands{3,2}(2)));
grid on;

% -------- Panel 5: Beta --------
subplot(3,2,5); hold on;
plot(results.NN.f,   results.NN.C_squared,  'Color', col_NN,   'LineWidth', 1.6);
plot(results.aHH.f,  results.aHH.C_squared, 'Color', col_aHH,  'LineWidth', 1.6);
plot(results.HH48.f, results.HH48.C_squared,'Color', col_HH48, 'LineWidth', 1.6);
yline(results.HH48.S_threshold, '--', 'Color', col_thr, 'LineWidth', 1.0);
xlim(bands{4,2});
xlabel('Frequency (Hz)');
ylabel('Coherence (a.u.)');
title(sprintf('%s (%g-%g Hz)', bands{4,1}, bands{4,2}(1), bands{4,2}(2)));
grid on;

% -------- Panel 6: Gamma + leyenda --------
subplot(3,2,6); hold on;
plot(results.NN.f,   results.NN.C_squared,  'Color', col_NN,   'LineWidth', 1.6);
plot(results.aHH.f,  results.aHH.C_squared, 'Color', col_aHH,  'LineWidth', 1.6);
plot(results.HH48.f, results.HH48.C_squared,'Color', col_HH48, 'LineWidth', 1.6);
yline(results.HH48.S_threshold, '--', 'Color', col_thr, 'LineWidth', 1.0);
xlim(bands{5,2});
xlabel('Frequency (Hz)');
ylabel('Coherence (a.u.)');
title(sprintf('%s (%g-%g Hz)', bands{5,1}, bands{5,2}(1), bands{5,2}(2)));
grid on;

% Leyenda única para toda la figura
lgd = legend([h1 h2 h3 h4], {'NN', 'aHH', '48-h HH', 'Threshold'}, ...
    'Orientation', 'horizontal', 'Box', 'off');
lgd.Position = [0.36 0.01 0.30 0.03];

saveas(fig34, fullfile(out_path, 'Figure_3_4_Combined_Coherence.png'));

%% =========================
%  GUARDAR MÉTRICAS EN TABLA
% ==========================
summary_tbl = table;
summary_tbl.Condition = {'NN'; 'aHH'; '48-h HH'};

summary_tbl.Delta_mean = [results.NN.metrics.delta.mean_coherence;
                          results.aHH.metrics.delta.mean_coherence;
                          results.HH48.metrics.delta.mean_coherence];

summary_tbl.Theta_mean = [results.NN.metrics.theta.mean_coherence;
                          results.aHH.metrics.theta.mean_coherence;
                          results.HH48.metrics.theta.mean_coherence];

summary_tbl.Alpha_mean = [results.NN.metrics.alpha.mean_coherence;
                          results.aHH.metrics.alpha.mean_coherence;
                          results.HH48.metrics.alpha.mean_coherence];

summary_tbl.Beta_mean = [results.NN.metrics.beta.mean_coherence;
                         results.aHH.metrics.beta.mean_coherence;
                         results.HH48.metrics.beta.mean_coherence];

summary_tbl.Gamma_mean = [results.NN.metrics.gamma.mean_coherence;
                          results.aHH.metrics.gamma.mean_coherence;
                          results.HH48.metrics.gamma.mean_coherence];

writetable(summary_tbl, fullfile(out_path, 'jesseavilez_summary_metrics.xlsx'));

fprintf('\nListo. Figuras y tabla exportadas en:\n%s\n', out_path);

%% =========================
%  FUNCIÓN LOCAL
% ==========================
function R = process_emg_and_coherence(file, nombre_triceps, nombre_biceps, ...
    hp_cutoff, lp_cutoff, fs_target, segment_duration_sec, ...
    window, noverlap, nfft, alpha_sig)

    a = importdata(file);

    idx_triceps = find(strcmp(a.channelNames, nombre_triceps));
    idx_biceps  = find(strcmp(a.channelNames, nombre_biceps));

    if isempty(idx_triceps) || isempty(idx_biceps)
        error('No se encontraron los canales requeridos en %s', file);
    end

    triceps_raw_all = cell2mat(a.Data(:, idx_triceps));
    biceps_raw_all  = cell2mat(a.Data(:, idx_biceps));

    fs_raw = a.samplingRate;

    inicio_idx = round(a.Markers(1,1) * fs_raw);
    duracion_muestras = round(180 * fs_raw);

    idx_end = inicio_idx + duracion_muestras - 1;
    if idx_end > length(triceps_raw_all) || idx_end > length(biceps_raw_all)
        error('El archivo %s no tiene suficientes muestras para recortar 180 s desde el marcador.', file);
    end

    triceps_raw = triceps_raw_all(inicio_idx:idx_end);
    biceps_raw  = biceps_raw_all(inicio_idx:idx_end);

    % Paso 1: high-pass
    [b_hp, a_hp] = butter(4, hp_cutoff / (fs_raw / 2), 'high');
    triceps_hp = filtfilt(b_hp, a_hp, triceps_raw);
    biceps_hp  = filtfilt(b_hp, a_hp, biceps_raw);

    % Paso 2: Hilbert envelope
    triceps_env = abs(hilbert(triceps_hp));
    biceps_env  = abs(hilbert(biceps_hp));

    % Paso 3: low-pass
    [b_lp, a_lp] = butter(4, lp_cutoff / (fs_raw / 2), 'low');
    triceps_filt = filtfilt(b_lp, a_lp, triceps_env);
    biceps_filt  = filtfilt(b_lp, a_lp, biceps_env);

    % Paso 4: resample
    [p, q] = rat(fs_target / fs_raw);
    triceps_ds = resample(triceps_filt, p, q);
    biceps_ds  = resample(biceps_filt, p, q);

    % Paso 5: normalize
    triceps_norm = (triceps_ds - mean(triceps_ds)) / std(triceps_ds);
    biceps_norm  = (biceps_ds  - mean(biceps_ds))  / std(biceps_ds);

    % Paso 6: segmentation
    segment_length = round(segment_duration_sec * fs_target);
    n_segments = floor(length(triceps_norm) / segment_length);

    triceps_segments = zeros(segment_length, n_segments);
    biceps_segments  = zeros(segment_length, n_segments);

    for i = 1:n_segments
        idx_start = (i - 1) * segment_length + 1;
        idx_stop  = idx_start + segment_length - 1;
        triceps_segments(:, i) = triceps_norm(idx_start:idx_stop);
        biceps_segments(:, i)  = biceps_norm(idx_start:idx_stop);
    end

    % coherence
    n_freq = nfft/2 + 1;
    Sxx_all = zeros(n_freq, n_segments);
    Syy_all = zeros(n_freq, n_segments);
    Sxy_all = zeros(n_freq, n_segments);

    for w = 1:n_segments
        x = triceps_segments(:, w);
        y = biceps_segments(:, w);

        [Sxx, f] = pwelch(x, window, noverlap, nfft, fs_target);
        [Syy, ~] = pwelch(y, window, noverlap, nfft, fs_target);
        [Sxy, ~] = cpsd(x, y, window, noverlap, nfft, fs_target);

        Sxx_all(:, w) = Sxx;
        Syy_all(:, w) = Syy;
        Sxy_all(:, w) = Sxy;
    end

    Sxx_avg = mean(Sxx_all, 2);
    Syy_avg = mean(Syy_all, 2);
    Sxy_avg = mean(Sxy_all, 2);

    C_complex = Sxy_avg ./ sqrt(Sxx_avg .* Syy_avg);
    C_squared = abs(C_complex).^2;

    % band metrics
    bands_local = {
        'delta', [1 4];
        'theta', [4 8];
        'alpha', [8 12];
        'beta',  [13 30];
        'gamma', [30 45];
    };

    L = n_segments;
    S_threshold = 1 - alpha_sig^(1 / (L - 1));

    metrics = struct();
    for i = 1:size(bands_local, 1)
        band_name = bands_local{i,1};
        f_range = bands_local{i,2};
        idx_band = find(f >= f_range(1) & f <= f_range(2));
        band_coh = C_squared(idx_band);

        metrics.(band_name).mean_coherence = mean(band_coh);
        metrics.(band_name).max_coherence  = max(band_coh);
        metrics.(band_name).A_c            = sum(band_coh > S_threshold) / length(band_coh);
    end

    % output
    R.file = file;
    R.fs_raw = fs_raw;
    R.fs_ds = fs_target;
    R.f = f;
    R.C_squared = C_squared;
    R.S_threshold = S_threshold;
    R.metrics = metrics;

    R.triceps_raw = triceps_raw;
    R.biceps_raw  = biceps_raw;
    R.triceps_hp = triceps_hp;
    R.biceps_hp  = biceps_hp;
    R.triceps_env = triceps_env;
    R.biceps_env  = biceps_env;
    R.triceps_filt = triceps_filt;
    R.biceps_filt  = biceps_filt;
    R.triceps_norm = triceps_norm;
    R.biceps_norm  = biceps_norm;
end

