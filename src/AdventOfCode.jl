module AdventOfCode

@enum Day begin
    day01
end

function runday01()
    include(joinpath(@__DIR__, "Days/Day01.jl"))
    solve("hi")
end

end
