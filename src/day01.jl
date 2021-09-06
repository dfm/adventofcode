struct Coord
    x::Int
    y::Int
end

function turn(p::Coord, dir::Char)
    dir == 'L' ? Coord(-p.y, p.x) : Coord(p.y, -p.x)
end

function step(coord::Coord, pointing::Coord, amt::Int)
    Coord(coord.x + pointing.x * amt, coord.y + pointing.y * amt)
end

function move(coord::Coord, pointing::Coord, dir::Char, amt::Int)
    pointing = turn(pointing, dir)
    step(coord, pointing, amt), pointing
end
    
function load(path)
    open(path, "r") do io
        split(strip(read(io, String)), ", ") 
    end
end

function solve1(tokens)
    coord = Coord(0, 0)
    pointing = Coord(0, 1)
    for token = tokens
        dir = token[1]
        amt = parse(Int, token[2:length(token)])
        coord, pointing = move(coord, pointing, dir, amt)
    end
    abs(coord.x) + abs(coord.y)
end

function solve2(tokens)
    n = 1
    coord = Coord(0, 0)
    pointing = Coord(0, 1)
    sofar = Set([coord])
    while true
        token = tokens[n]
        dir = token[1]
        amt = parse(Int, token[2:length(token)])
        pointing = turn(pointing, dir)
        for k = 1:amt
            coord = step(coord, pointing, 1)
            if coord ∈ sofar
                return abs(coord.x) + abs(coord.y)
            end
            push!(sofar, coord)
        end
        n = mod(n, length(tokens)) + 1
    end
    0
end


tokens = load("data/day01/input.txt")
println(solve1(tokens))
println(solve2(tokens))
