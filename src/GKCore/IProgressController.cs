using System;

namespace GKCore
{
	public interface IProgressController
	{
		void ProgressInit(int max, string title);
		void ProgressDone();

		void ProgressStep();
		void ProgressStep(int value);
		
		/*bool IsAborting
		{
			get;
		}*/

		/*int Progress
		{
			get;
			set;
		}*/
	}
}
