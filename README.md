# ParliamentVotes
A Power BI dashboard to investigate the 44th Canadian Parliament

Select the vote you're interested in to visualize the vote via the seating map.

Note: The "% Whipped" metric is defined as the percentage of votes for which a given member voted in alignment with their party leader (if one existed during that vote), excluding cases where votes were 
paired with another member of parliament. Because of this, leaders of parties which had not had a previous leader during this parliament
(Trudeau, Blanchet, Singh, and May) will always have a value of 100%. Conversely, groups which never had leaders (independents) will always have a value of 0%. 
In the case of the Green Party, which had a leader for only a portion of this parliament, only votes during which they had a leader contributed to this metric.


Be aware that this metric DOES include cases whether both members did not vote. As a particular example of this, note Speaker 
Anthony Rota's % Whipped metric: since the speaker does not vote, he coincidentally "agrees" with Justin Trudeau whenever Justin Trudeau does not vote. I opted
to include "did not vote" cases in this metric as opposed to excluding it like with paired votes because there are times where members may be instructed not to vote
by their caucus. Following or ignoring this instruction would then say something about whether this member's action has been whipped.

![Example Screenshot](https://user-images.githubusercontent.com/56042923/230809680-a0459078-7f67-4982-bffb-a794a59d28c2.PNG)
