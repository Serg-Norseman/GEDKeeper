using System;

namespace GKCore
{
	public interface IProgressController
	{
		void ProgressInit(int aMax, string aTitle);
		void ProgressDone();
		void ProgressStep();
		
		/*int Progress
		{
			get;
			set;
		}*/
	}
}
