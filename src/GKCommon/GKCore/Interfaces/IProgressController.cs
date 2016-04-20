namespace GKCore.Interfaces
{
	public interface IProgressController
	{
		void ProgressInit(string title, int max);
		void ProgressDone();

		void ProgressStep();
		void ProgressStep(int value);
		
		/*bool IsAborting
		{
			get;
		}*/
	}
}
