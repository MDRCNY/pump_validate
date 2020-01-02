% This is a sample Matlab script
 
% Import dependencies

% Print to console/stdout
printf(strcat("hello, ",getenv('DOMINO_PROJECT_OWNER'),"! \n\n"))


% Plot the values of random points
yn = randn(10000,1);
hist(yn)
print('-f1','-djpeg','results/myHistogramFromMatlab.jpg')

% Generate and save some key statistics to dominostats.json
%   learn more at http://support.dominodatalab.com/hc/en-us/articles/204348169
r2 = rand(1)
p = rand(1)
file = fopen('dominostats.json','w');
jsonAsString = strcat('{"R^2":',num2str(r2),', "p-value":', num2str(p),'}')
fprintf(file,jsonAsString)