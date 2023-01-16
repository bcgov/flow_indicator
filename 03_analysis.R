# Copyright 2023 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

library(EnvStats)

# Least Squares trend line.
lm.fit <- lm(Value ~ Year, data=station.mean)
summary(lm.fit)$coefficients

# Mann-Kendall trend test.
mk.test <- kendallTrendTest(Value~Year, data=station.mean)
mk.test
cat("Confidence interval for the slope \n")
mk.test$interval$limits

# Tau is the MK correlation between the year and value variables.
cor.test(station.mean$Value, station.mean$Year, method="kendall")

# "Linear" trend line of Senn slope.
station.mean$mk.pred <- mk.test$estimate["intercept"]+
  mk.test$estimate["slope"]*station.mean$Year

plot1 %>% add_trace(data=station.mean, x=~Year, y=~mk.pred, mode="lines")

# Note: we may prefer to use a version of the MK test that accounts for
# serial autocorrelation. This function shows the old and new p values and
# other parameters of the model fit.
mmky.test <- modifiedmk::mmky1lag(station.mean$Value)
mmky.test

# We could also use a 'robust' linear regression model.
lmrob.fit <- robustbase::lmrob(Value ~ Year, data=station.mean)
summary(lmrob.fit)$coefficients

station.mean$lmrob.pred <- predict(lmrob.fit, newdata=station.mean)

plot1 %>% add_trace(data=station.mean, x=~Year, y=~lmrob.pred, mode="lines")
