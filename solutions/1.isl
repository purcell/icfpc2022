color[0][0,74,173,255]
cut[0][360,40]
color[0.3][255,255,255,255]
cut[0.3][200,200]
color[0.3.0][0,0,0,255]
color[0.3.2][0,0,0,255]
merge[0.3.0][0.3.3]
merge[0.3.1][0.3.2]
cut[1][X][40]
cut[1.1][X][80]
cut[1.1.1][X][120]
cut[1.1.1.1][X][160]
cut[2][X][240]
cut[2.1][X][280]
cut[2.1.1][X][320]
swap[1.1.0][2.1.0]
swap[1.1.1.1.0][2.1.1.1]
merge[1.0][2.1.0]
merge[3][1.1.1.0]
merge[4][2.1.1.1]
merge[5][1.1.1.1.1]
merge[6][2.0]
merge[7][1.1.0]
merge[8][2.1.1.0]
merge[9][1.1.1.1.0]
cut[10][Y][80]
cut[10.1][Y][120]
cut[10.1.1][Y][160]
cut[10.1.1.1][Y][200]
cut[10.1.1.1.1][Y][240]
cut[10.1.1.1.1.1][Y][280]
cut[10.1.1.1.1.1.1][Y][320]
cut[10.1.1.1.1.1.1.1][Y][360]
swap[10.0][10.1.1.1.1.1.0]
swap[10.1.1.0][10.1.1.1.1.1.1.1.0]