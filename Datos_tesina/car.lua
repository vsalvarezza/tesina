-- Car profile

api_version = 4

Set = require('lib/set')
Sequence = require('lib/sequence')
Handlers = require("lib/way_handlers")
Relations = require("lib/relations")
find_access_tag = require("lib/access").find_access_tag
limit = require("lib/maxspeed").limit
Utils = require("lib/utils")
Measure = require("lib/measure")

function setup()
  return {
    properties = {
      max_speed_for_map_matching      = 180/3.6, -- 180kmph -> m/s
      -- For routing based on duration, but weighted for preferring certain roads
      -- weight_name                     = 'routability',
      -- For shortest duration without penalties for accessibility
      -- weight_name                     = 'duration',
      -- For shortest distance without penalties for accessibility
      weight_name                     = 'distance',
      process_call_tagless_node      = false,
      u_turn_penalty                 = 20,
      continue_straight_at_waypoint  = true,
      use_turn_restrictions          = true,
      left_hand_driving              = false,
      traffic_light_penalty          = 2,
    },

    default_mode              = mode.driving,
    default_speed             = 10,
    oneway_handling           = true,
    side_road_multiplier      = 0.8,
    turn_penalty              = 7.5,
    speed_reduction           = 0.8,
    turn_bias                 = 1.075,
    cardinal_directions       = false,

    -- Size of the vehicle, to be limited by physical restriction of the way
    vehicle_height = 2.0, -- in meters, 2.0m is the height slightly above biggest SUVs
    vehicle_width = 1.9, -- in meters, ways with narrow tag are considered narrower than 2.2m

    -- Size of the vehicle, to be limited mostly by legal restriction of the way
    vehicle_length = 4.8, -- in meters, 4.8m is the length of large or family car
    vehicle_weight = 2000, -- in kilograms

    -- a list of suffixes to suppress in name change instructions. The suffixes also include common substrings of each other
    suffix_list = {
      'N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW', 'North', 'South', 'West', 'East', 'Nor', 'Sou', 'We', 'Ea'
    },

    barrier_whitelist = Set {
      'cattle_grid',
      'border_control',
      'toll_booth',
      'sally_port',
      'gate',
      'lift_gate',
      'no',
      'entrance',
      'height_restrictor',
      'arch'
    },

    access_tag_whitelist = Set {
      'yes',
      'motorcar',
      'motor_vehicle',
      'vehicle',
      'permissive',
      'designated',
      'hov'
    },

    access_tag_blacklist = Set {
      'no',
      'agricultural',
      'forestry',
      'emergency',
      'psv',
      'customers',
      'private',
      'delivery',
      'destination'
    },

    -- tags disallow access to in combination with highway=service
    service_access_tag_blacklist = Set {
        'private'
    },

    restricted_access_tag_list = Set {
      'private',
      'delivery',
      'destination',
      'customers',
    },

    access_tags_hierarchy = Sequence {
      'motorcar',
      'motor_vehicle',
      'vehicle',
      'access'
    },

    service_tag_forbidden = Set {
      'emergency_access'
    },

    restrictions = Sequence {
      'motorcar',
      'motor_vehicle',
      'vehicle'
    },

    classes = Sequence {
        'toll', 'motorway', 'ferry', 'restricted', 'tunnel'
    },

    -- classes to support for exclude flags
    excludable = Sequence {
        Set {'toll'},
        Set {'motorway'},
        Set {'ferry'}
    },

    avoid = Set {
      'area',
      -- 'toll',    -- uncomment this to avoid tolls
      'reversible',
      'impassable',
      'hov_lanes',
      'steps',
      'construction',
      'proposed'
    },

    speeds = Sequence {
  highway = {
    motorway        = 40,  -- Reduce la velocidad de las autopistas
    motorway_link   = 40,  -- Reduce la velocidad de los enlaces de autopistas
    trunk           = 40,  -- Reduce la velocidad de las rutas principales
    trunk_link      = 40,  -- Reduce la velocidad de los enlaces de rutas principales
    primary         = 40,  -- Reduce la velocidad de las carreteras primarias
    primary_link    = 40,  -- Reduce la velocidad de los enlaces de carreteras primarias
    secondary       = 40,  -- Reduce la velocidad de las carreteras secundarias
    secondary_link  = 40,  -- Reduce la velocidad de los enlaces de carreteras secundarias
    tertiary        = 40,  -- Reduce la velocidad de las carreteras terciarias
    tertiary_link   = 40,  -- Reduce la velocidad de los enlaces de carreteras terciarias
    unclassified    = 40,  -- Reduce la velocidad de las carreteras no clasificadas
    residential     = 40,  -- Reduce la velocidad de las calles residenciales
    living_street   = 40,  -- Reduce la velocidad en calles de viviendas
    service         = 40   -- Reduce la velocidad en calles de servicio
  }
},


    service_penalties = {
      alley             = 0.5,
      parking           = 0.5,
      parking_aisle     = 0.5,
      driveway          = 0.5,
      ["drive-through"] = 0.5,
      ["drive-thru"] = 0.5
    },

    restricted_highway_whitelist = Set {
      'motorway',
      'motorway_link',
      'trunk',
      'trunk_link',
      'primary',
      'primary_link',
      'secondary',
      'secondary_link',
      'tertiary',
      'tertiary_link',
      'residential',
      'living_street',
      'unclassified',
      'service'
    },

    construction_whitelist = Set {
      'no',
      'widening',
      'minor',
    },

     route_speeds = {
      ferry = 5,
      shuttle_train = 10
     },

     bridge_speeds = {
      movable = 5
     },

    -- surface/trackype/smoothness
    -- values were estimated from looking at the photos at the relevant wiki pages

    -- max speed for surfaces
     surface_speeds = {
       asphalt = nil,    -- nil mean no limit. removing the line has the same effect
       concrete = nil,
       ["concrete:plates"] = nil,
       ["concrete:lanes"] = nil,
       paved = nil,

       cement = 40,
       compacted = 40,
       fine_gravel = 40,

       paving_stones = 40,
       metal = 40,
       bricks = 40,

       grass = 40,
       wood = 40,
       sett = 40,
       grass_paver = 40,
       gravel = 40,
       unpaved = 40,
       ground = 40,
       dirt = 40,
       pebblestone = 40,
       tartan = 40,

       cobblestone = 40,
       clay = 40,

       earth = 40,
       stone = 40,
       rocky = 40,
       sand = 40,

       mud = 40
     },

    -- max speed for tracktypes
     tracktype_speeds = {
       grade1 =  40,
       grade2 =  40,
       grade3 =  40,
       grade4 =  40,
       grade5 =  40
     },

    -- max speed for smoothnesses
     smoothness_speeds = {
       intermediate    =  40,
       bad             =  40,
       very_bad        =  40,
       horrible        =  40,
       very_horrible   =  40,
       impassable      =  40
     },

    -- http://wiki.openstreetmap.org/wiki/Speed_limits
     maxspeed_table_default = {
       urban = 40,
       rural = 40,
       trunk = 40,
       motorway = 40
     },

    -- List only exceptions
     maxspeed_table = {
      ["at:rural"] = 40,
      ["at:trunk"] = 40,
      ["be:motorway"] = 40,
      ["be-bru:rural"] = 40,
      ["be-bru:urban"] = 40,
      ["be-vlg:rural"] = 40,
      ["by:urban"] = 40,
      ["by:motorway"] = 40,
      ["ch:rural"] = 40,
      ["ch:trunk"] = 40,
      ["ch:motorway"] = 40,
      ["cz:trunk"] = 40,
      ["cz:motorway"] = 40,
      ["de:living_street"] = 40,
      ["de:rural"] = 40,
      ["de:motorway"] = 40,
      ["dk:rural"] = 40,
      ["fr:rural"] = 40,
      ["gb:nsl_single"] = 40,
      ["gb:nsl_dual"] = 40,
      ["gb:motorway"] = 40,
      ["nl:rural"] = 40,
      ["nl:trunk"] = 40,
      ['no:rural'] = 40,
      ['no:motorway'] = 40,
      ['pl:rural'] = 40,
      ['pl:trunk'] = 40,
      ['pl:motorway'] = 40,
      ["ro:trunk"] = 40,
      ["ru:living_street"] = 40,
      ["ru:urban"] = 40,
      ["ru:motorway"] = 40,
      ["uk:nsl_single"] = 40,
      ["uk:nsl_dual"] = 40,
      ["uk:motorway"] = 40,
      ['za:urban'] = 40,
      ['za:rural'] = 40,
      ["none"] = 40
     },

    relation_types = Sequence {
      "route"
    },

    -- classify highway tags when necessary for turn weights
    highway_turn_classification = {
    },

    -- classify access tags when necessary for turn weights
    access_turn_classification = {
    }
  }
end

function process_way(profile, way, result, relations)
  -- the intial filtering of ways based on presence of tags
  -- affects processing times significantly, because all ways
  -- have to be checked.
  -- to increase performance, prefetching and intial tag check
  -- is done in directly instead of via a handler.

  -- in general we should  try to abort as soon as
  -- possible if the way is not routable, to avoid doing
  -- unnecessary work. this implies we should check things that
  -- commonly forbids access early, and handle edge cases later.

  -- data table for storing intermediate values during processing
  local data = {
    -- prefetch tags
    highway = way:get_value_by_key('highway'),
    bridge = way:get_value_by_key('bridge'),
    route = way:get_value_by_key('route')
  }

  -- perform an quick initial check and abort if the way is
  -- obviously not routable.
  -- highway or route tags must be in data table, bridge is optional
  if (not data.highway or data.highway == '') and
  (not data.route or data.route == '')
  then
    return
  end

  handlers = Sequence {
    -- set the default mode for this profile. if can be changed later
    -- in case it turns we're e.g. on a ferry
    WayHandlers.default_mode,

    -- check various tags that could indicate that the way is not
    -- routable. this includes things like status=impassable,
    -- toll=yes and oneway=reversible
    WayHandlers.blocked_ways,
    WayHandlers.avoid_ways,
    WayHandlers.handle_height,
    WayHandlers.handle_width,
    WayHandlers.handle_length,
    WayHandlers.handle_weight,

    -- determine access status by checking our hierarchy of
    -- access tags, e.g: motorcar, motor_vehicle, vehicle
    WayHandlers.access,

    -- check whether forward/backward directions are routable
    WayHandlers.oneway,

    -- check a road's destination
    WayHandlers.destinations,

    -- check whether we're using a special transport mode
    WayHandlers.ferries,
    WayHandlers.movables,

    -- handle service road restrictions
    WayHandlers.service,

    -- handle hov
    WayHandlers.hov,

    -- compute speed taking into account way type, maxspeed tags, etc.
    WayHandlers.set_distance,  -- Nueva función para priorizar la distancia
    -- WayHandlers.maxspeed,
    -- WayHandlers.surface,
    -- WayHandlers.penalties,

    -- compute class labels
    WayHandlers.classes,

    -- handle turn lanes and road classification, used for guidance
    WayHandlers.turn_lanes,
    -- WayHandlers.classification,

    -- handle various other flags
    WayHandlers.roundabouts,
    WayHandlers.startpoint,
    WayHandlers.driving_side,

    -- set name, ref and pronunciation
    WayHandlers.names,

    -- set weight properties of the way
    WayHandlers.weights,

    -- set classification of ways relevant for turns
    WayHandlers.way_classification_for_turn
  }
end

function process_turn(profile, turn)
  -- Use a sigmoid function to return a penalty that maxes out at turn_penalty
  -- over the space of 0-180 degrees.  Values here were chosen by fitting
  -- the function to some turn penalty samples from real driving.
  local turn_penalty = profile.turn_penalty
  local turn_bias = turn.is_left_hand_driving and 1. / profile.turn_bias or profile.turn_bias

  if turn.has_traffic_light then
      turn.duration = profile.properties.traffic_light_penalty
  end

  if turn.number_of_roads > 2 or turn.source_mode ~= turn.target_mode or turn.is_u_turn then
    if turn.angle >= 0 then
      turn.duration = turn.duration + turn_penalty / (1 + math.exp( -((13 / turn_bias) *  turn.angle/180 - 6.5*turn_bias)))
    else
      turn.duration = turn.duration + turn_penalty / (1 + math.exp( -((13 * turn_bias) * -turn.angle/180 - 6.5/turn_bias)))
    end

    if turn.is_u_turn then
      turn.duration = turn.duration + profile.properties.u_turn_penalty
    end
  end

  -- for distance based routing we don't want to have penalties based on turn angle
  if profile.properties.weight_name == 'distance' then
     turn.weight = 0
  else
     turn.weight = turn.duration
  end

  --if profile.properties.weight_name == 'routability' then
      -- penalize turns from non-local access only segments onto local access only tags
      --if not turn.source_restricted and turn.target_restricted then
          --turn.weight = constants.max_turn_weight
      --end
  --end
end

return {
  setup = setup,
  process_way = process_way,
  process_node = process_node,
  process_turn = process_turn
}
