function db2ewpgdata()

ylim=100000;
close all
load data0;
i0=1;
i1=length(data0);
figure(1);
length(data0)
subplot(2,1,1),plot(data0(i0:i1),'r');
%set(gca, 'YLim', [ -ylim ylim]);
load data1;
length(data1)
subplot(2,1,2),plot(data1(i0:i1),'b');
%set(gca, 'YLim', [ -ylim ylim]);

%%%%%%%%%%%%%%%%%%%%%%%%%%
minmax(data1);
load data2;
minmax(data2);
load data3;
minmax(data3);
load data4;
minmax(data4);
load data5;
minmax(data5);
load data6;
minmax(data6);

figure(3);
subplot(3,2,1),plot(data1(i0:i1),'r');
%set(gca, 'YLim', [ -ylim ylim]);

subplot(3,2,2),plot(data2(i0:i1),'g');
%set(gca, 'YLim', [ -ylim ylim]);

subplot(3,2,3),plot(data3(i0:i1),'b');
%set(gca, 'YLim', [ -ylim ylim]);

subplot(3,2,4),plot(data4(i0:i1),'c');
%set(gca, 'YLim', [ -ylim ylim]);

subplot(3,2,5),plot(data5(i0:i1),'m');
%set(gca, 'YLim', [ -ylim ylim]);

subplot(3,2,6),plot(data6(i0:i1),'y');

function minmax(data)
mind = min(data);
maxd = max(data);
disp(sprintf('%e to %e',mind,maxd));


