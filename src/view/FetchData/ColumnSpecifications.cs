
using System;

namespace DIPolWeb.FetchData;

internal abstract record ColumnSpecification
{
    public abstract bool Matches(DipolObservation obs);
}

internal record ObjectSpecification(Condition<string> Condition) : ColumnSpecification
{
    public override bool Matches(DipolObservation obs) => Condition.Matches(obs.Object);
}

internal record YearSpecification(Condition<int> Condition) : ColumnSpecification
{
    public override bool Matches(DipolObservation obs) => Condition.Matches(obs.Date.Year);
}

internal record DateSpecification(Condition<DateOnly> Condition) : ColumnSpecification
{
    public override bool Matches(DipolObservation obs) => Condition.Matches(obs.Date);
}

internal record InstrumentSpecification(Condition<string> Condition) : ColumnSpecification
{
    public override bool Matches(DipolObservation obs) => Condition.Matches(obs.Instrument);
}

internal record TelescopeSpecification(Condition<string> Condition) : ColumnSpecification
{
    public override bool Matches(DipolObservation obs) => Condition.Matches(obs.Telescope);
}