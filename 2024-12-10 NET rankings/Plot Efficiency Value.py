#code source: https://github.com/bchare
#this code was not written by maddy

import pandas as pd
import numpy as np
# import matplotlib as mpl
import matplotlib.pyplot as plt
from matplotlib.offsetbox import OffsetImage, AnnotationBbox
import math

season = 2025

# Read the output from "Estimate NET.py"
net_stats = pd.read_csv('estimated_net_output.csv')
net_stats['Conference'] = "Other"
net_stats.loc[net_stats['team'].isin(["Boston College","California","Clemson","Duke","Florida St.","Georgia Tech","Louisville","Miami (FL)","NC State","North Carolina","Notre Dame","Pittsburgh","SMU","Stanford","Syracuse","Virginia","Virginia Tech","Wake Forest"]), 'Conference'] = "ACC"
net_stats.loc[net_stats['team'].isin(["Butler","Creighton","DePaul","Georgetown","Marquette","Providence","Seton Hall","St. John\'s (NY)","UConn","Villanova","Xavier"]), 'Conference'] = "Big East"
net_stats.loc[net_stats['team'].isin(["Arizona","Arizona St.","Baylor","BYU","Cincinnati","Colorado","Houston","Iowa St.","Kansas","Kansas St.","Oklahoma St.","TCU","Texas Tech","UCF","Utah","West Virginia"]), 'Conference'] = "Big 12"
net_stats.loc[net_stats['team'].isin(["Alabama","Auburn","Arkansas","Florida","Georgia","Kentucky","LSU","Mississippi St.","Missouri","Ole Miss","Oklahoma","South Carolina","Tennessee","Texas","Texas A&M","Vanderbilt"]), 'Conference'] = "SEC"
net_stats.loc[net_stats['team'].isin(["Illinois","Indiana","Iowa","Maryland","Michigan","Michigan St.","Minnesota","Nebraska","Northwestern","Ohio St.","Oregon","Penn St.","Purdue","Rutgers","Southern California","Washington","Wisconsin","UCLA"]), 'Conference'] = "Big Ten"
net_stats.loc[net_stats['team'].isin(["Gonzaga","LMU (CA)","Oregon St.","Pacific","Pepperdine","Portland","Saint Mary's (CA)","San Diego","San Francisco","Santa Clara","Washington St."]), 'Conference'] = "WCC"
net_stats.loc[net_stats['team'].isin(["Air Force","Boise St.","Colorado St.","Fresno St.","Nevada","New Mexico","San Diego St.","San Jose St.","UNLV","Utah St.","Wyoming"]), 'Conference'] = "Mountain West"
net_stats.loc[net_stats['team'].isin(["Campbell","Col. of Charleston","Delaware","Drexel","Elon","Hampton","Hofstra","Monmouth","N.C. A&T","Northeastern","Stony Brook","Towson","UNCW","William & Mary"]), 'Conference'] = "CAA"
net_stats.loc[net_stats['team'].isin(["Charlotte","East Carolina","Fla. Atlantic","Memphis","North Texas","Rice","SMU","South Fla.","Temple","Tulane","Tulsa","UAB","UTSA","Wichita St."]), 'Conference'] = "AAC"
net_stats.loc[net_stats['team'].isin(["Belmont","Bradley","Drake","Evansville","Illinois St.","Indiana St.","Missouri St.","Murray St.","Southern Ill.","UIC","UNI","Valparaiso"]), 'Conference'] = "MVC"
net_stats.loc[net_stats['team'].isin(["Cal Poly","Cal St. Fullerton","CSU Bakersfield","CSUN","Hawaii","Long Beach St.","UC Davis","UC Irvine","UC Riverside","UC San Diego","UC Santa Barbara"]), 'Conference'] = "Big West"
net_stats.loc[net_stats['team'].isin(["FIU","Jacksonville St.","Liberty","Louisiana Tech","Middle Tenn.","New Mexico St.","Sam Houston","UTEP","Western Ky."]), 'Conference'] = "CUSA"
net_stats.loc[net_stats['team'].isin(["Abilene Christian","California Baptist","Grand Canyon","Seattle U","SFA","Southern Utah","Tarleton St.","UT Arlington","Utah Tech","Utah Valley","UTRGV"]), 'Conference'] = "WAC"
net_stats.loc[net_stats['team'].isin(["Davidson","Dayton","Duquesne","Fordham","George Mason","George Washington","La Salle","Loyola Chicago","Massachusetts","Rhode Island","Richmond","Saint Joseph's","Saint Louis","St. Bonaventure","VCU"]), 'Conference'] = "Atlantic 10"
net_stats.loc[net_stats['team'].isin(["Chattanooga","ETSU","Furman","Mercer","Samford","The Citadel","UNC Greensboro","VMI","Western Caro.","Wofford"]), 'Conference'] = "SoCon"

# net_stats.loc[net_stats['team'].isin([]), 'Conference'] = ""

# average_net_by_conference = net_stats.groupby('Conference')['estimated_net'].mean().reset_index()
# average_net_by_conference['Rank'] = average_net_by_conference['estimated_net'].rank(ascending=True)
# average_net_by_conference = average_net_by_conference.sort_values('Rank').reset_index(drop=True)
# print(average_net_by_conference)

# Input the real NET values
# https://stats.ncaa.org/selection_rankings/season_divisions/18221/nitty_gritties?utf8=%E2%9C%93&commit=Submit
# realnet = pd.read_table('actual_net.txt')
# net_stats = pd.merge(net_stats, realnet, left_on='team', right_on='Team')
# net_stats['to_display'] = '#' + net_stats['NET'].astype(str)

# For the graph title, use the list of games to find the most recent date
try:
    games = pd.read_csv(f'ncaab_stats_input_net_{season}.csv')
    # games = games[games['date'] <= '2024-03-17']
    maxdate = games['date'].max()
except:
    maxdate = str(season)

# Recommended: Add team colors
try:
    teamcolors = pd.read_csv('teamcolors.csv')
    net_stats = net_stats.merge(teamcolors, on='team')
except:
    net_stats['bgcolor'] = '#000000'

# If you have logos for every team, point to them
try:
    teamlogos = pd.read_csv('teamlogos.csv')
    teamlogos = teamlogos[teamlogos['thruyear'] == season]
    teamlogos['logo'] = '.\\logos\\'+teamlogos['logo']
    net_stats = net_stats.merge(teamlogos, on='team')
    # net_stats['to_display'] = '#' + net_stats['NET'].astype(str)
    net_stats['to_display'] = '#' + net_stats['estimated_net'].astype(str)    
    havelogos = 1
except:
    # net_stats['to_display'] = '#' + net_stats['NET'].astype(str) + ' ' + net_stats['team']
    net_stats['to_display'] = '#' + net_stats['estimated_net'].astype(str) + ' ' + net_stats['team']    
    havelogos = 0

def makeplot(title, condition):
    # Sorting them this way puts circles for the #1 team over others
    # net_stats.sort_values(by='NET', ascending=False, inplace=True)
    net_stats.sort_values(by='estimated_net', ascending=False, inplace=True)
    net_stats['visibility'] = net_stats.eval(condition).astype(int)
    net_stats_subset=net_stats[net_stats['visibility']==1]
    max_x = 5*math.ceil(net_stats['value_rtg'].max()/5)
    max_y = 5*math.ceil(net_stats['efficiency_rtg'].max()/5)
    min_x = 5*math.floor(net_stats_subset['value_rtg'].min()/5)
    min_y = 5*math.floor(net_stats_subset['efficiency_rtg'].min()/5)
    fig, ax = plt.subplots(figsize=(12.8, 7.2), dpi=100, layout='constrained')
    # fig.suptitle(('NET - ' + title + '\n(Games Through ' + maxdate + ')').strip())
    #fig.suptitle(('Estimated NET - ' + title + '\n(Games Through ' + maxdate + ')').strip())
    fig.suptitle(('Estimated NET - ' + title + '\n(Games Through 12/9/2024' ')').strip())
    ax.scatter(net_stats['value_rtg'], net_stats['efficiency_rtg'], 100, net_stats['bgcolor'], alpha=0.05*(1-net_stats['visibility']))
    if havelogos == 1:
        for i in net_stats_subset.index:
            ax.annotate(net_stats_subset.at[i,'to_display'], xy=(net_stats_subset.at[i,'value_rtg'], net_stats_subset.at[i,'efficiency_rtg']), ha='center', va='bottom', xytext=(0, 15), textcoords='offset points', zorder=5000)
            ax.add_artist(AnnotationBbox(OffsetImage(plt.imread(net_stats_subset.at[i,'logo']), zoom=0.30), (net_stats_subset.at[i,'value_rtg'], net_stats_subset.at[i,'efficiency_rtg']), frameon=False))
    else:
        ax.scatter(net_stats_subset['value_rtg'], net_stats_subset['efficiency_rtg'], 100, net_stats_subset['bgcolor'], alpha=1)
        for i in net_stats_subset.index:
            ax.annotate(net_stats_subset.at[i,'to_display'], xy=(net_stats_subset.at[i,'value_rtg'], net_stats_subset.at[i,'efficiency_rtg']), ha='center', va='bottom', xytext=(0, 6), textcoords='offset points', zorder=5000)
    ax.set_ylabel('Efficiency Rating')
    ax.set_xlabel('Value Rating')
    ax.set_xticks(np.arange(min_x, max_x+1, 5))
    ax.set_yticks(np.arange(min_y, max_y+1, 5))
    ax.axis([min_x-1, max_x+1, min_y-1, max_y+1])
    fig.savefig(f'NET Scatter {title}.png')

# makeplot("All Teams", "NET <= 999")
makeplot("Top 25", "estimated_net <= 25")
makeplot("Top 50", "estimated_net <= 50")
makeplot("Top 75", "estimated_net <= 75")
makeplot("Top 100", "estimated_net <= 100")
makeplot("ACC", "Conference == 'ACC'")
makeplot("Big East", "Conference == 'Big East'")
makeplot("Big 12", "Conference == 'Big 12'")
makeplot("SEC", "Conference == 'SEC'")
makeplot("Big Ten", "Conference == 'Big Ten'")

# makeplot("NC Teams", "state == 'North Carolina'")

