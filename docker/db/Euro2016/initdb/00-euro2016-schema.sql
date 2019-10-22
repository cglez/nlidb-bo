--
-- PostgreSQL database dump
--

-- Dumped from database version 11.5 (Debian 11.5-1.pgdg90+1)
-- Dumped by pg_dump version 11.5 (Debian 11.5-1.pgdg90+1)

-- Started on 2019-09-28 08:01:54 UTC

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- TOC entry 211 (class 1259 OID 16607)
-- Name: asst_referee_mast; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.asst_referee_mast (
    ass_ref_id numeric NOT NULL,
    ass_ref_name character varying(40) NOT NULL,
    country_id numeric NOT NULL
);


ALTER TABLE public.asst_referee_mast OWNER TO postgres;

--
-- TOC entry 241 (class 1259 OID 16727)
-- Name: goal_details; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.goal_details (
    goal_id numeric NOT NULL,
    match_no numeric NOT NULL,
    player_id numeric NOT NULL,
    team_id numeric NOT NULL,
    goal_time numeric NOT NULL,
    goal_type character(1) NOT NULL,
    play_stage character(1) NOT NULL,
    goal_schedule character(2) NOT NULL,
    goal_half numeric
);


ALTER TABLE public.goal_details OWNER TO postgres;

--
-- TOC entry 259 (class 1259 OID 16795)
-- Name: match_captain; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.match_captain (
    match_no numeric NOT NULL,
    team_id numeric NOT NULL,
    player_captain numeric NOT NULL
);


ALTER TABLE public.match_captain OWNER TO postgres;

--
-- TOC entry 260 (class 1259 OID 16801)
-- Name: match_details; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.match_details (
    match_no numeric NOT NULL,
    play_stage character(1) NOT NULL,
    team_id numeric NOT NULL,
    win_lose character(1) NOT NULL,
    decided_by character(1) NOT NULL,
    goal_score numeric NOT NULL,
    penalty_score numeric,
    ass_ref numeric NOT NULL,
    player_gk numeric NOT NULL
);


ALTER TABLE public.match_details OWNER TO postgres;

--
-- TOC entry 261 (class 1259 OID 16807)
-- Name: match_mast; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.match_mast (
    match_no numeric NOT NULL,
    play_stage character(1) NOT NULL,
    play_date date NOT NULL,
    results character(5) NOT NULL,
    decided_by character(1) NOT NULL,
    goal_score character(5) NOT NULL,
    venue_id numeric NOT NULL,
    referee_id numeric NOT NULL,
    audence numeric NOT NULL,
    plr_of_match numeric NOT NULL,
    stop1_sec numeric NOT NULL,
    stop2_sec numeric NOT NULL
);


ALTER TABLE public.match_mast OWNER TO postgres;

--
-- TOC entry 306 (class 1259 OID 16973)
-- Name: penalty_gk; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.penalty_gk (
    match_no numeric NOT NULL,
    team_id numeric NOT NULL,
    player_gk numeric NOT NULL
);


ALTER TABLE public.penalty_gk OWNER TO postgres;

--
-- TOC entry 307 (class 1259 OID 16979)
-- Name: penalty_shootout; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.penalty_shootout (
    kick_id numeric NOT NULL,
    match_no numeric NOT NULL,
    team_id numeric NOT NULL,
    player_id numeric NOT NULL,
    score_goal character(1) NOT NULL,
    kick_no numeric NOT NULL
);


ALTER TABLE public.penalty_shootout OWNER TO postgres;

--
-- TOC entry 310 (class 1259 OID 16997)
-- Name: player_booked; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.player_booked (
    match_no numeric NOT NULL,
    team_id numeric NOT NULL,
    player_id numeric NOT NULL,
    booking_time numeric NOT NULL,
    sent_off character(1) DEFAULT NULL::bpchar,
    play_schedule character(2) NOT NULL,
    play_half numeric NOT NULL
);


ALTER TABLE public.player_booked OWNER TO postgres;

--
-- TOC entry 311 (class 1259 OID 17004)
-- Name: player_in_out; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.player_in_out (
    match_no numeric NOT NULL,
    team_id numeric NOT NULL,
    player_id numeric NOT NULL,
    in_out character(1) NOT NULL,
    time_in_out numeric NOT NULL,
    play_schedule character(2) NOT NULL,
    play_half numeric NOT NULL
);


ALTER TABLE public.player_in_out OWNER TO postgres;

--
-- TOC entry 312 (class 1259 OID 17010)
-- Name: player_mast; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.player_mast (
    player_id numeric NOT NULL,
    team_id numeric NOT NULL,
    jersey_no numeric NOT NULL,
    player_name character varying(40) NOT NULL,
    posi_to_play character(2) NOT NULL,
    dt_of_bir date,
    age numeric,
    playing_club character varying(40)
);


ALTER TABLE public.player_mast OWNER TO postgres;

--
-- TOC entry 313 (class 1259 OID 17016)
-- Name: playing_position; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.playing_position (
    position_id character(2) NOT NULL,
    position_desc character varying(15) NOT NULL
);


ALTER TABLE public.playing_position OWNER TO postgres;

--
-- TOC entry 318 (class 1259 OID 17039)
-- Name: referee_mast; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.referee_mast (
    referee_id numeric NOT NULL,
    referee_name character varying(40) NOT NULL,
    country_id numeric NOT NULL
);


ALTER TABLE public.referee_mast OWNER TO postgres;

--
-- TOC entry 333 (class 1259 OID 17094)
-- Name: soccer_city; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.soccer_city (
    city_id numeric NOT NULL,
    city character varying(25) NOT NULL,
    country_id numeric NOT NULL
);


ALTER TABLE public.soccer_city OWNER TO postgres;

--
-- TOC entry 334 (class 1259 OID 17100)
-- Name: soccer_country; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.soccer_country (
    country_id numeric NOT NULL,
    country_abbr character varying(4) NOT NULL,
    country_name character varying(40) NOT NULL
);


ALTER TABLE public.soccer_country OWNER TO postgres;

--
-- TOC entry 335 (class 1259 OID 17106)
-- Name: soccer_team; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.soccer_team (
    team_id numeric NOT NULL,
    team_group character(1) NOT NULL,
    match_played numeric NOT NULL,
    won numeric NOT NULL,
    draw numeric NOT NULL,
    lost numeric NOT NULL,
    goal_for numeric NOT NULL,
    goal_agnst numeric NOT NULL,
    goal_diff numeric NOT NULL,
    points numeric NOT NULL,
    group_position numeric NOT NULL
);


ALTER TABLE public.soccer_team OWNER TO postgres;

--
-- TOC entry 336 (class 1259 OID 17112)
-- Name: soccer_venue; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.soccer_venue (
    venue_id numeric NOT NULL,
    venue_name character varying(30) NOT NULL,
    city_id numeric NOT NULL,
    aud_capacity numeric NOT NULL
);


ALTER TABLE public.soccer_venue OWNER TO postgres;

--
-- Name: coach_mast; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.coach_mast (
    coach_id numeric NOT NULL,
    coach_name character varying(40) NOT NULL
);


ALTER TABLE public.coach_mast OWNER TO postgres;

--
-- TOC entry 344 (class 1259 OID 17151)
-- Name: team_coaches; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.team_coaches (
    team_id numeric NOT NULL,
    coach_id numeric NOT NULL
);


ALTER TABLE public.team_coaches OWNER TO postgres;

--
-- TOC entry 3625 (class 2606 OID 17238)
-- Name: asst_referee_mast asst_referee_mast_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.asst_referee_mast
    ADD CONSTRAINT asst_referee_mast_pkey PRIMARY KEY (ass_ref_id);


--
-- TOC entry 3627 (class 2606 OID 17270)
-- Name: goal_details goal_details_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.goal_details
    ADD CONSTRAINT goal_details_pkey PRIMARY KEY (goal_id);


--
-- TOC entry 3629 (class 2606 OID 17286)
-- Name: match_mast match_mast_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.match_mast
    ADD CONSTRAINT match_mast_pkey PRIMARY KEY (match_no);


--
-- TOC entry 3631 (class 2606 OID 17306)
-- Name: penalty_shootout penalty_shootout_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.penalty_shootout
    ADD CONSTRAINT penalty_shootout_pkey PRIMARY KEY (kick_id);


--
-- TOC entry 3633 (class 2606 OID 17310)
-- Name: player_mast player_mast_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.player_mast
    ADD CONSTRAINT player_mast_pkey PRIMARY KEY (player_id);


--
-- TOC entry 3635 (class 2606 OID 17312)
-- Name: playing_position playing_position_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.playing_position
    ADD CONSTRAINT playing_position_pkey PRIMARY KEY (position_id);


--
-- TOC entry 3637 (class 2606 OID 17318)
-- Name: referee_mast referee_mast_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.referee_mast
    ADD CONSTRAINT referee_mast_pkey PRIMARY KEY (referee_id);


--
-- TOC entry 3639 (class 2606 OID 17330)
-- Name: soccer_city soccer_city_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.soccer_city
    ADD CONSTRAINT soccer_city_pkey PRIMARY KEY (city_id);


--
-- TOC entry 3641 (class 2606 OID 17332)
-- Name: soccer_country soccer_country_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.soccer_country
    ADD CONSTRAINT soccer_country_pkey PRIMARY KEY (country_id);


--
-- TOC entry 3643 (class 2606 OID 17334)
-- Name: soccer_venue soccer_venue_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.soccer_venue
    ADD CONSTRAINT soccer_venue_pkey PRIMARY KEY (venue_id);


--
-- TOC entry 3651 (class 2606 OID 17345)
-- Name: match_details ass_ref_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.match_details
    ADD CONSTRAINT ass_ref_fkey FOREIGN KEY (ass_ref) REFERENCES public.asst_referee_mast(ass_ref_id);


--
-- TOC entry 3675 (class 2606 OID 17350)
-- Name: soccer_venue city_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.soccer_venue
    ADD CONSTRAINT city_id_fkey FOREIGN KEY (city_id) REFERENCES public.soccer_city(city_id);


--
-- Name: coach_mast_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.coach_mast
    ADD CONSTRAINT coach_mast_pkey PRIMARY KEY (coach_id);


--
-- TOC entry 3676 (class 2606 OID 17355)
-- Name: team_coaches coach_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.team_coaches
    ADD CONSTRAINT coach_id_fkey FOREIGN KEY (coach_id) REFERENCES public.coach_mast(coach_id);


--
-- TOC entry 3673 (class 2606 OID 17360)
-- Name: soccer_city country_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.soccer_city
    ADD CONSTRAINT country_id_fkey FOREIGN KEY (country_id) REFERENCES public.soccer_country(country_id);


--
-- TOC entry 3672 (class 2606 OID 17365)
-- Name: referee_mast country_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.referee_mast
    ADD CONSTRAINT country_id_fkey FOREIGN KEY (country_id) REFERENCES public.soccer_country(country_id);


--
-- TOC entry 3644 (class 2606 OID 17370)
-- Name: asst_referee_mast country_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.asst_referee_mast
    ADD CONSTRAINT country_id_fkey FOREIGN KEY (country_id) REFERENCES public.soccer_country(country_id);


--
-- TOC entry 3652 (class 2606 OID 17505)
-- Name: match_details match_no_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.match_details
    ADD CONSTRAINT match_no_fkey FOREIGN KEY (match_no) REFERENCES public.match_mast(match_no);


--
-- TOC entry 3664 (class 2606 OID 17510)
-- Name: player_booked match_no_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.player_booked
    ADD CONSTRAINT match_no_fkey FOREIGN KEY (match_no) REFERENCES public.match_mast(match_no);


--
-- TOC entry 3667 (class 2606 OID 17515)
-- Name: player_in_out match_no_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.player_in_out
    ADD CONSTRAINT match_no_fkey FOREIGN KEY (match_no) REFERENCES public.match_mast(match_no);


--
-- TOC entry 3661 (class 2606 OID 17520)
-- Name: penalty_shootout match_no_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.penalty_shootout
    ADD CONSTRAINT match_no_fkey FOREIGN KEY (match_no) REFERENCES public.match_mast(match_no);


--
-- TOC entry 3645 (class 2606 OID 17525)
-- Name: goal_details match_no_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.goal_details
    ADD CONSTRAINT match_no_fkey FOREIGN KEY (match_no) REFERENCES public.match_mast(match_no);


--
-- TOC entry 3648 (class 2606 OID 17530)
-- Name: match_captain match_no_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.match_captain
    ADD CONSTRAINT match_no_fkey FOREIGN KEY (match_no) REFERENCES public.match_mast(match_no);


--
-- TOC entry 3658 (class 2606 OID 17535)
-- Name: penalty_gk match_no_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.penalty_gk
    ADD CONSTRAINT match_no_fkey FOREIGN KEY (match_no) REFERENCES public.match_mast(match_no);


--
-- TOC entry 3649 (class 2606 OID 17570)
-- Name: match_captain player_captain_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.match_captain
    ADD CONSTRAINT player_captain_fkey FOREIGN KEY (player_captain) REFERENCES public.player_mast(player_id);


--
-- TOC entry 3653 (class 2606 OID 17575)
-- Name: match_details player_gk_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.match_details
    ADD CONSTRAINT player_gk_fkey FOREIGN KEY (player_gk) REFERENCES public.player_mast(player_id);


--
-- TOC entry 3659 (class 2606 OID 17580)
-- Name: penalty_gk player_gk_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.penalty_gk
    ADD CONSTRAINT player_gk_fkey FOREIGN KEY (player_gk) REFERENCES public.player_mast(player_id);


--
-- TOC entry 3665 (class 2606 OID 17585)
-- Name: player_booked player_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.player_booked
    ADD CONSTRAINT player_id_fkey FOREIGN KEY (player_id) REFERENCES public.player_mast(player_id);


--
-- TOC entry 3668 (class 2606 OID 17590)
-- Name: player_in_out player_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.player_in_out
    ADD CONSTRAINT player_id_fkey FOREIGN KEY (player_id) REFERENCES public.player_mast(player_id);


--
-- TOC entry 3662 (class 2606 OID 17595)
-- Name: penalty_shootout player_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.penalty_shootout
    ADD CONSTRAINT player_id_fkey FOREIGN KEY (player_id) REFERENCES public.player_mast(player_id);


--
-- TOC entry 3646 (class 2606 OID 17600)
-- Name: goal_details player_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.goal_details
    ADD CONSTRAINT player_id_fkey FOREIGN KEY (player_id) REFERENCES public.player_mast(player_id);


--
-- TOC entry 3655 (class 2606 OID 17605)
-- Name: match_mast plr_of_match_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.match_mast
    ADD CONSTRAINT plr_of_match_fkey FOREIGN KEY (plr_of_match) REFERENCES public.player_mast(player_id);


--
-- TOC entry 3670 (class 2606 OID 17610)
-- Name: player_mast posi_to_play_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.player_mast
    ADD CONSTRAINT posi_to_play_fkey FOREIGN KEY (posi_to_play) REFERENCES public.playing_position(position_id);


--
-- TOC entry 3656 (class 2606 OID 17625)
-- Name: match_mast referee_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.match_mast
    ADD CONSTRAINT referee_id_fkey FOREIGN KEY (referee_id) REFERENCES public.referee_mast(referee_id);


--
-- TOC entry 3677 (class 2606 OID 17640)
-- Name: team_coaches team_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.team_coaches
    ADD CONSTRAINT team_id_fkey FOREIGN KEY (team_id) REFERENCES public.soccer_country(country_id);


--
-- TOC entry 3671 (class 2606 OID 17645)
-- Name: player_mast team_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.player_mast
    ADD CONSTRAINT team_id_fkey FOREIGN KEY (team_id) REFERENCES public.soccer_country(country_id);


--
-- TOC entry 3674 (class 2606 OID 17650)
-- Name: soccer_team team_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.soccer_team
    ADD CONSTRAINT team_id_fkey FOREIGN KEY (team_id) REFERENCES public.soccer_country(country_id);


--
-- TOC entry 3654 (class 2606 OID 17655)
-- Name: match_details team_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.match_details
    ADD CONSTRAINT team_id_fkey FOREIGN KEY (team_id) REFERENCES public.soccer_country(country_id);


--
-- TOC entry 3666 (class 2606 OID 17660)
-- Name: player_booked team_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.player_booked
    ADD CONSTRAINT team_id_fkey FOREIGN KEY (team_id) REFERENCES public.soccer_country(country_id);


--
-- TOC entry 3669 (class 2606 OID 17665)
-- Name: player_in_out team_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.player_in_out
    ADD CONSTRAINT team_id_fkey FOREIGN KEY (team_id) REFERENCES public.soccer_country(country_id);


--
-- TOC entry 3663 (class 2606 OID 17670)
-- Name: penalty_shootout team_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.penalty_shootout
    ADD CONSTRAINT team_id_fkey FOREIGN KEY (team_id) REFERENCES public.soccer_country(country_id);


--
-- TOC entry 3647 (class 2606 OID 17675)
-- Name: goal_details team_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.goal_details
    ADD CONSTRAINT team_id_fkey FOREIGN KEY (team_id) REFERENCES public.soccer_country(country_id);


--
-- TOC entry 3650 (class 2606 OID 17680)
-- Name: match_captain team_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.match_captain
    ADD CONSTRAINT team_id_fkey FOREIGN KEY (team_id) REFERENCES public.soccer_country(country_id);


--
-- TOC entry 3660 (class 2606 OID 17685)
-- Name: penalty_gk team_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.penalty_gk
    ADD CONSTRAINT team_id_fkey FOREIGN KEY (team_id) REFERENCES public.soccer_country(country_id);


--
-- TOC entry 3657 (class 2606 OID 17690)
-- Name: match_mast venue_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.match_mast
    ADD CONSTRAINT venue_id_fkey FOREIGN KEY (venue_id) REFERENCES public.soccer_venue(venue_id);


-- Completed on 2019-09-28 08:01:55 UTC

--
-- PostgreSQL database dump complete
--

