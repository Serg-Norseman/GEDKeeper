using GDModel;
using NUnit.Framework;

namespace GKCore.Validation
{
    [TestFixture]
    public class ValidationTests
    {
        [Test]
        public void Test_InvalidEvent()
        {
            var indiEvent = new GDMIndividualEvent() { Cause = "91characters+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++" };

            var results = ValidationFactory.Validate(indiEvent as GDMCustomEvent);

            Assert.IsNotNull(results);
            Assert.IsFalse(results.Valid);
            Assert.IsTrue(results.Messages.Count > 0);
        }
    }
}
