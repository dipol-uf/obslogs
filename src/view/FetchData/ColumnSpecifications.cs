
using System;
using DIPolWeb.FetchData.Json;

namespace DIPolWeb.FetchData;

internal abstract record ColumnSpecification
{
    public abstract bool Matches(IData obs);
}

internal record ObjectSpecification(Condition<string> Condition) : ColumnSpecification
{
    public override bool Matches(IData obs) => Condition.Matches(obs.Object);
}

internal record DateSpecification(Condition<DateOnly> Condition) : ColumnSpecification
{
    public override bool Matches(IData obs) => Condition.Matches(obs.Date);
}

internal record InstrumentSpecification(Condition<string> Condition) : ColumnSpecification
{
    public override bool Matches(IData obs) => Condition.Matches(obs.Instrument);
}

internal record TelescopeSpecification(Condition<string> Condition) : ColumnSpecification
{
    public override bool Matches(IData obs) => Condition.Matches(obs.Telescope);
}