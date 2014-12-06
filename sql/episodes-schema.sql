--
-- PostgreSQL database dump
--

SET statement_timeout = 0;
SET lock_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: account; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE account (
    id bigint NOT NULL,
    nickname character varying(64),
    email character varying(64),
    password character varying(512),
    admin boolean NOT NULL,
    views bigint NOT NULL,
    created timestamp with time zone NOT NULL,
    modified timestamp with time zone NOT NULL,
    accessed_approx timestamp with time zone NOT NULL
);


--
-- Name: account_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE account_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: account_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE account_id_seq OWNED BY account.id;


--
-- Name: episode; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE episode (
    id bigint NOT NULL,
    number bigint NOT NULL,
    title character varying(512) NOT NULL,
    season bigint NOT NULL,
    air_date_time timestamp with time zone NOT NULL,
    view_count bigint NOT NULL,
    created timestamp with time zone NOT NULL,
    modified timestamp with time zone NOT NULL
);


--
-- Name: episode_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE episode_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: episode_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE episode_id_seq OWNED BY episode.id;


--
-- Name: episode_status; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE episode_status (
    id bigint NOT NULL,
    account bigint NOT NULL,
    episode bigint NOT NULL,
    status character varying(32) NOT NULL,
    created timestamp with time zone NOT NULL,
    modified timestamp with time zone NOT NULL
);


--
-- Name: episode_status_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE episode_status_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: episode_status_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE episode_status_id_seq OWNED BY episode_status.id;


--
-- Name: profile; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE profile (
    id bigint NOT NULL,
    account bigint NOT NULL,
    timezone character varying(128),
    cookie character varying(256),
    episode_links character varying(8196),
    created timestamp with time zone NOT NULL,
    modified timestamp with time zone NOT NULL
);


--
-- Name: profile_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE profile_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: profile_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE profile_id_seq OWNED BY profile.id;


--
-- Name: season; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE season (
    id bigint NOT NULL,
    number bigint NOT NULL,
    show bigint NOT NULL,
    created timestamp with time zone NOT NULL,
    modified timestamp with time zone NOT NULL
);


--
-- Name: season_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE season_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: season_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE season_id_seq OWNED BY season.id;


--
-- Name: show; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE show (
    id bigint NOT NULL,
    title character varying(256) NOT NULL,
    tv_rage_id bigint,
    subscription_count bigint NOT NULL,
    created timestamp with time zone NOT NULL,
    modified timestamp with time zone NOT NULL,
    next_update timestamp with time zone NOT NULL,
    last_update timestamp with time zone NOT NULL
);


--
-- Name: show_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE show_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: show_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE show_id_seq OWNED BY show.id;


--
-- Name: subscription; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE subscription (
    id bigint NOT NULL,
    account bigint NOT NULL,
    show bigint NOT NULL,
    created timestamp with time zone NOT NULL,
    modified timestamp with time zone NOT NULL
);


--
-- Name: subscription_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE subscription_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: subscription_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE subscription_id_seq OWNED BY subscription.id;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY account ALTER COLUMN id SET DEFAULT nextval('account_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY episode ALTER COLUMN id SET DEFAULT nextval('episode_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY episode_status ALTER COLUMN id SET DEFAULT nextval('episode_status_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY profile ALTER COLUMN id SET DEFAULT nextval('profile_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY season ALTER COLUMN id SET DEFAULT nextval('season_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY show ALTER COLUMN id SET DEFAULT nextval('show_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY subscription ALTER COLUMN id SET DEFAULT nextval('subscription_id_seq'::regclass);


--
-- Name: account_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY account
    ADD CONSTRAINT account_pkey PRIMARY KEY (id);


--
-- Name: episode_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY episode
    ADD CONSTRAINT episode_pkey PRIMARY KEY (id);


--
-- Name: episode_status_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY episode_status
    ADD CONSTRAINT episode_status_pkey PRIMARY KEY (id);


--
-- Name: profile_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY profile
    ADD CONSTRAINT profile_pkey PRIMARY KEY (id);


--
-- Name: season_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY season
    ADD CONSTRAINT season_pkey PRIMARY KEY (id);


--
-- Name: show_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY show
    ADD CONSTRAINT show_pkey PRIMARY KEY (id);


--
-- Name: subscription_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY subscription
    ADD CONSTRAINT subscription_pkey PRIMARY KEY (id);


--
-- Name: unique_account_email; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY account
    ADD CONSTRAINT unique_account_email UNIQUE (email);


--
-- Name: unique_account_nickname; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY account
    ADD CONSTRAINT unique_account_nickname UNIQUE (nickname);


--
-- Name: unique_episode_status_account_episode; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY episode_status
    ADD CONSTRAINT unique_episode_status_account_episode UNIQUE (account, episode);


--
-- Name: unique_profile_account; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY profile
    ADD CONSTRAINT unique_profile_account UNIQUE (account);


--
-- Name: unique_profile_cookie; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY profile
    ADD CONSTRAINT unique_profile_cookie UNIQUE (cookie);


--
-- Name: unique_season_episode_number; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY episode
    ADD CONSTRAINT unique_season_episode_number UNIQUE (season, number);


--
-- Name: unique_show_season_number; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY season
    ADD CONSTRAINT unique_show_season_number UNIQUE (show, number);


--
-- Name: unique_show_t_v_rage_id; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY show
    ADD CONSTRAINT unique_show_t_v_rage_id UNIQUE (tv_rage_id);


--
-- Name: unique_subscription_account_show; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY subscription
    ADD CONSTRAINT unique_subscription_account_show UNIQUE (account, show);


--
-- Name: account_email_i; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE UNIQUE INDEX account_email_i ON account USING btree (email);


--
-- Name: account_nickname_i; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE UNIQUE INDEX account_nickname_i ON account USING btree (nickname);


--
-- Name: episode_air_date_time_i; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE INDEX episode_air_date_time_i ON episode USING btree (air_date_time);


--
-- Name: episode_season_i; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE INDEX episode_season_i ON episode USING btree (season);


--
-- Name: episode_title_i; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE INDEX episode_title_i ON episode USING btree (title);


--
-- Name: show_last_update_i; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE INDEX show_last_update_i ON show USING btree (last_update);


--
-- Name: show_next_update_i; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE INDEX show_next_update_i ON show USING btree (next_update);


--
-- Name: show_subscription_count_i; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE INDEX show_subscription_count_i ON show USING btree (subscription_count);


--
-- Name: show_title_i; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE INDEX show_title_i ON show USING btree (title);


--
-- Name: show_tv_rage_id_i; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE UNIQUE INDEX show_tv_rage_id_i ON show USING btree (tv_rage_id);


--
-- Name: episode_season_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY episode
    ADD CONSTRAINT episode_season_fkey FOREIGN KEY (season) REFERENCES season(id);


--
-- Name: episode_status_account_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY episode_status
    ADD CONSTRAINT episode_status_account_fkey FOREIGN KEY (account) REFERENCES account(id);


--
-- Name: episode_status_episode_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY episode_status
    ADD CONSTRAINT episode_status_episode_fkey FOREIGN KEY (episode) REFERENCES episode(id);


--
-- Name: profile_account_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY profile
    ADD CONSTRAINT profile_account_fkey FOREIGN KEY (account) REFERENCES account(id);


--
-- Name: season_show_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY season
    ADD CONSTRAINT season_show_fkey FOREIGN KEY (show) REFERENCES show(id);


--
-- Name: subscription_account_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY subscription
    ADD CONSTRAINT subscription_account_fkey FOREIGN KEY (account) REFERENCES account(id);


--
-- Name: subscription_show_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY subscription
    ADD CONSTRAINT subscription_show_fkey FOREIGN KEY (show) REFERENCES show(id);


--
-- PostgreSQL database dump complete
--

