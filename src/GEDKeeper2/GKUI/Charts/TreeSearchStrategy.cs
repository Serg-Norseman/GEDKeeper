using System;
using GKCore;
using GKCore.Interfaces;

namespace GKUI.Charts
{
	public class TreeSearchStrategy : BaseSearchStrategy
	{
		public TreeSearchStrategy(IWorkWindow workWindow, String searchPattern) : base(workWindow, searchPattern)
		{
		}
	}
}
