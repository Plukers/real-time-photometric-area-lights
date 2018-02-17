function rmse = RMSE(img_ref, img_dist)
%
%
%      rmse = RMSE(img_ref, img_dist)
%
%
%       Input:
%           -img_ref: input reference image
%           -img_dist: input distorted image
%
%       Output:
%           -mse: the Root Mean Squared Error assuming values in [0,1]. Lower
%           values means better quality.
% 
%     Copyright (C) 2016  Francesco Banterle
%
%     This program is free software: you can redistribute it and/or modify
%     it under the terms of the GNU General Public License as published by
%     the Free Software Foundation, either version 3 of the License, or
%     (at your option) any later version.
% 
%     This program is distributed in the hope that it will be useful,
%     but WITHOUT ANY WARRANTY; without even the implied warranty of
%     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%     GNU General Public License for more details.
% 
%     You should have received a copy of the GNU General Public License
%     along with this program.  If not, see <http://www.gnu.org/licenses/>.
%

checkNegative(img_ref);
checkNegative(img_dist);

rmse = sqrt(MSE(img_ref, img_dist));

end
