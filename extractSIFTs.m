%% Setup
% Change the current folder to the folder of this m-file.
path = '~/Dropbox/DM/ASL/Project/';
cd(path);

% Change this to VLfeat location
vl_dir = '~/Downloads/vlfeat-0.9.20' ;

% Setup vlfeat for matlab
run(strcat(vl_dir, '/toolbox/vl_setup'));

%% Extract features
delta = 8; % sampling step
gamma = 0.018; % contrast threshold

% Loop through 2 dataset
for num=1:2
    path = strcat('VOC2005_', int2str(num), '/');
    img_path = strcat(path, 'PNGImages/');
    
    desc_path = strcat(path, 'DescSIFTs/');
    if (exist(desc_path)==0)
        mkdir(desc_path);
    end
    
    kp_path = strcat(path, 'kpSIFTs/');
    if (exist(kp_path)==0)
        mkdir(kp_path);
    end
    
    % Get subdir list
    listdir = dir(img_path);
    
    % Loop through subdir
    for cat=3:(size(listdir,1))
        folder = listdir(cat).name;
        img_dir = strcat(img_path, folder, '/');
        
        desc_dir = strcat(desc_path, folder, '/');
        if (exist(desc_dir)==0)
            mkdir(desc_dir);
        end
    
        kp_dir = strcat(kp_path, folder, '/');
        if (exist(kp_dir)==0)
            mkdir(kp_dir);
        end
        
        % Get image list
        imgs = dir([img_dir '*.png']);
        
        % Loop through images
        for im=1:size(imgs,1)
            I = imread(strcat(img_dir,imgs(im).name)) ;
            
            % Extract dense descriptors
            I_single = im2single(I);
            [f,d] = vl_phow(I_single, 'ContrastThreshold', gamma, 'Step', delta) ;
            
            % Plot keypoints
            kp=[];
            for i=1:size(d,2)
                if(sum(d(:,i))~=0)
                    kp=[kp,f(1:2,i)];
                end
            end
            
            f = figure('visible','off');
            image(I)
            hold on
            scatter(kp(1,:), kp(2,:), 'red')
            hold off
            saveas(f, strcat(kp_dir,imgs(im).name(1:end-4)), 'png');
            
            % Save descriptors
            save(strcat(desc_dir, imgs(im).name(1:end-4), '.mat'), 'd');
            
        end
    end
    
end

