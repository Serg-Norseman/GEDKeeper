using GKCore.Types;

namespace GKCore.Interfaces
{
	public interface ISubscriber
	{
		void NotifyRecord(IBaseWindow aBase, object record, RecordAction action);
	}
}
